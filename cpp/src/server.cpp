
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include <csignal>
#include <iostream>
#include <random>
#include <string>
#include <sstream>

#include "utils.hpp"


/***************************************************************************************************
 * Arguments for program
 **************************************************************************************************/

struct Arguments
{
  std::string broadcast_host;
  uint16_t    broadcast_port;
  unsigned    instance_id;
  float       period;
};


/***************************************************************************************************
 * Function Declarations
 **************************************************************************************************/

int parse_args( int argc, char *argv[], Arguments *parsed_args );
void print_usage( void );
void signal_handler( int signum );


/***************************************************************************************************
 * Constants
 **************************************************************************************************/

bool STOP = false;


/***************************************************************************************************
 * Function Definitions
 **************************************************************************************************/

/**************************************************************************************************/
int main(int argc, char *argv[])
{
  signal(SIGINT, signal_handler);
  Arguments args = {};
  parse_args(argc, argv, &args);

  // Get socket and set SO_BROADCAST sockopt
  int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
  if (sockfd < 0)
  {
    std::cerr << "Could not create Socket.";
    exit(1);
  }
  uint32_t broadcast = 1;
  if (setsockopt(sockfd, SOL_SOCKET, SO_BROADCAST, &broadcast, sizeof(broadcast)) < 0)
  {
    std::cerr << "Could not set socket to broadcast mode." << std::endl;
    exit(1);
  }

  // Set Destination Address
  struct sockaddr_in broadcast_addr = {};
  broadcast_addr.sin_family      = AF_INET;
  broadcast_addr.sin_addr.s_addr = inet_addr(args.broadcast_host.c_str());
  broadcast_addr.sin_port        = htons(args.broadcast_port);

  // Set up run loop
  int num_msgs = 1;

  // Run loop
  while (!STOP)
  {
    float rand      = random_float();
    float send_time = args.period * rand; //random_float();
    float wait_time = args.period - send_time;
    std::cout << rand << " - " << send_time << " - " << wait_time << std::endl;
    usleep(float_to_useconds(send_time));
    std::stringstream msg_stream;
    msg_stream << "C++ Server["<< args.instance_id <<"] msg #"<<num_msgs<<";;;;";
    msg_stream << "C++ Server["<< args.instance_id <<"] 0:0 => "<<args.broadcast_host<<":"<<args.broadcast_port<<";;;;";
    std::string msg = msg_stream.str();
    sendto(sockfd, msg.c_str(), msg.length(), 0, (sockaddr*)&broadcast_addr, sizeof(broadcast_addr));
    std::cout << "Sent msg("<<num_msgs<<")"<< std::endl;
    num_msgs++;
    usleep(float_to_useconds(wait_time));
  }

  close(sockfd);
  return 0;
}

/**************************************************************************************************/
void signal_handler( int signum )
{
  std::cout << std::endl << "Received Signal." << std::endl;
  STOP = true;
}

/**************************************************************************************************/
void print_usage( void )
{
  std::cout << std::endl;
  std::cout << " USAGE:" << std::endl;
  std::cout << " $ server [-p <PERIOD>] <IP_ADDRESS> <INSTANCE_ID>" << std::endl;
  std::cout << std::endl;
  std::cout << " Positional Arguments:" << std::endl;
  std::cout << "   <IP_ADDRESS> is of the form <IPv4_Address>:<Port_Number>." << std::endl;
  std::cout << "   <INSTANCE_ID> is an int value to identify this server process." << std::endl;
  std::cout << std::endl;
  std::cout << " Optional Arguments:" << std::endl;
  std::cout << "   -p, --period <PERIOD>" << std::endl;
  std::cout << "     <PERIOD> number of seconds during which a message will send." << std::endl;
  std::cout << std::endl;
}

/**************************************************************************************************/
int parse_args( int argc, char *argv[], Arguments *parsed_args )
{
  std::string       arg;
  unsigned num_args_parsed = 0;
  if ( argc-1 < 2 )
  {
    std::cerr << "***Error: Encountered too few arguments." << std::endl;
    print_usage();
    exit(1);
  }
  for (unsigned i_arg = 1; i_arg < argc; )
  {
    arg = std::string(argv[i_arg]);
    // Parse optional args first
    if (arg.compare("-p") == 0 || arg.compare("--period") == 0)
    {
      arg = std::string( argv[i_arg+1] );
      parse( arg, parsed_args->period );
      i_arg += 2;
    }
    // First positional arg is the IP Address <IP_HOST>:<IP_PORT>
    else if (num_args_parsed == 0)
    {
      unsigned split_pos = arg.find_first_of(":");
      parse( arg.substr(0, split_pos), parsed_args->broadcast_host );
      parse( arg.substr(split_pos+1),  parsed_args->broadcast_port );
      num_args_parsed++;
      i_arg += 1;
    }
    // Second positional arg is INSTANCE_ID
    else if (num_args_parsed == 1)
    {
      parse( arg, parsed_args->instance_id );
      num_args_parsed++;
      i_arg += 1;
    }
    // Invalid Arg
    else
    {
      std::cerr << "***Error: Encountered unexpected argument." << std::endl;
      print_usage();
      exit(1);
    }
  }
  return 0;
}
