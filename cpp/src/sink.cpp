/**************************************************************************************************/

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include <csignal>
#include <iostream>
#include <string>
#include <sstream>

#include "utils.hpp"


/***************************************************************************************************
 * Arguments for program
 **************************************************************************************************/

struct Arguments
{
  uint16_t    listener_port;
};


/***************************************************************************************************
 * Function Declarations
 **************************************************************************************************/

int main( int argc, char *argv[] );
int parse_args( int argc, char *argv[], Arguments *parsed_args );
void signal_handler( int signum );
template<class InType, class OutType>
void parse( const InType &arg_in, OutType &value );


/***************************************************************************************************
 * Constants
 **************************************************************************************************/

bool STOP = false;
unsigned BUFFSIZE = 4096;


/***************************************************************************************************
 * Function Definitions
 **************************************************************************************************/

/**************************************************************************************************/
int main(int argc, char *argv[])
{
  signal(SIGINT, signal_handler);
  Arguments args = {};
  parse_args(argc, argv, &args);
  // Get socket
  int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
  if (sockfd < 0)
  {
    std::cerr << "Could not create Socket.";
    exit(1);
  }
  timeval timeout = {};
  timeout.tv_sec  = 1;
  if (setsockopt(sockfd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout)) < 0)
  {
    std::cerr << "Could not set receive timeout for socket." << std::endl;
    exit(1);
  }
  // Bind to listener socket
  struct sockaddr_in listener_addr = {};
  listener_addr.sin_family      = AF_INET;
  listener_addr.sin_addr.s_addr = INADDR_ANY;
  listener_addr.sin_port        = htons(args.listener_port);
  if (bind(sockfd,(sockaddr*)&listener_addr, sizeof(listener_addr)) < 0)
  {
    std::cerr << "Could not bind socket." << std::endl;
    exit(1);
  }
  // Set up run loop
  int num_msgs = 1;
  // Run loop
  char buffer[BUFFSIZE];
  while (!STOP)
  {
    unsigned msg_in_len;
    struct sockaddr_in recv_addr = {};
    if (0 > recvfrom(sockfd, buffer, BUFFSIZE, 0, (sockaddr*)&recv_addr, &msg_in_len))
    {
      continue;
    }
    std::string recv_host = std::string( inet_ntoa(recv_addr.sin_addr) );
    uint16_t    recv_port = ntohs(recv_addr.sin_port);
    std::string msg = std::string(buffer);
    std::string msg_separator = std::string(";;;;");
    std::cout << std::endl;
    for (unsigned start_pos = 0; start_pos < msg.length(); )
    {
      unsigned split_pos = msg.find( msg_separator, start_pos );
      std::cout << msg.substr( start_pos, split_pos-start_pos ) << std::endl;
      start_pos = split_pos + msg_separator.length();
    }
    std::cout << "C++ Sink msg #"<<num_msgs<< std::endl;
    std::cout << "C++ Sink "<<INADDR_ANY<<":"<<args.listener_port
              << " <= "<<recv_host<<":"<<recv_port << std::endl;
    num_msgs++;
  }
  // Close socket and exit
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
int parse_args( int argc, char *argv[], Arguments *parsed_args )
{
  std::string       arg;
  unsigned num_args_parsed = 0;
  for (unsigned i_arg = 1; i_arg < argc; )
  {
    arg = std::string(argv[i_arg]);
    // First positional arg is the Listener Port
    if (num_args_parsed == 0)
    {
      parse( arg, parsed_args->listener_port );
      num_args_parsed++;
      i_arg += 1;
    }
  }
  return 0;
}

/**************************************************************************************************/
template<class InType, class OutType>
void parse( const InType &arg_in, OutType &value )
{
  std::stringstream stream;
  stream << arg_in;
  stream >> value;
}


