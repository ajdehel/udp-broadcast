#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include <cstring>
#include <string>
#include <iostream>

#define MAX_BUFFSIZE 4096

/*******************************************************************************
 *  get_socket
 *  params:
 *    None
 *  returns:
 *    a socket file descriptor (a positive int)
 ******************************************************************************/
extern "C"
int get_socket( void )
{
  return socket(AF_INET, SOCK_DGRAM, 0);
}


/*******************************************************************************
 *  get_broadcast_socket
 *  params:
 *    None
 *  returns:
 *    a socket file descriptor (a positive int)
 ******************************************************************************/
extern "C"
int get_broadcast_socket( void )
{
  int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
  if ( sockfd < 0 )
    return sockfd;
  uint32_t broadcast = 1;
  int status = setsockopt( sockfd, SOL_SOCKET, SO_BROADCAST, &broadcast, sizeof(broadcast));
  if ( status == 0 )
    return sockfd;
  close(sockfd);
  return status;
}


/*******************************************************************************
 *  close_socket
 *  params:
 *    sockfd: int => socket file descriptor
 *  returns:
 *     0 on success
 *    -1 on failure
 ******************************************************************************/
extern "C"
int close_socket( int sockfd )
{
  return close(sockfd);
}


/*******************************************************************************
 *  bind_socket
 *  params:
 *    sockfd:  int      => socket file descriptor
 *    ip_host: char*    => IPv4 Address
 *    ip_port: short    => Port Number
 *  returns:
 *    0 on success
 ******************************************************************************/
extern "C"
int bind_socket( int *sockfd, char *ip_host, short ip_port )
{
  std::string host = std::string(ip_host);
  struct sockaddr_in bind_addr = {};
  bind_addr.sin_family      = AF_INET;
  bind_addr.sin_addr.s_addr = host.compare("0.0.0.0") ? INADDR_ANY : inet_addr(ip_host);
  bind_addr.sin_port        = htons(ip_port);
  return bind(*sockfd,(sockaddr*)&bind_addr, sizeof(bind_addr));
}


/*******************************************************************************
 *  send_msg
 *  params:
 *    sockfd:  int      => socket file descriptor
 *    ip_host: char*    => IPv4 Address to sent the message to
 *    ip_port: short    => Port to send the message to
 *    msg:     char*    => The message
 *    msg_len: unsigned => The length of the message
 *  returns:
 *    The number of bytes sent
 ******************************************************************************/
extern "C"
int send_msg( int sockfd, char *ip_host, short ip_port, char *msg, unsigned msg_len )
{
  struct sockaddr_in send_addr = {};
  send_addr.sin_family      = AF_INET;
  send_addr.sin_addr.s_addr = inet_addr(ip_host);
  send_addr.sin_port        = htons(ip_port);
  return sendto(sockfd, msg, msg_len, 0, (sockaddr*)&send_addr, sizeof(send_addr));
}


/*******************************************************************************
 *  broadcast_msg
 *  params:
 *    sockfd:  int      => socket file descriptor
 *    ip_host: char*    => IPv4 Broadcast Address to send the message to
 *    ip_port: short    => Port to broadcast the message on
 *    msg:     char*    => The message
 *    msg_len: unsigned => The length of the message
 *  returns:
 *    The number of bytes sent
 ******************************************************************************/
extern "C"
int broadcast_msg( int sockfd, char *ip_host, unsigned short ip_port, char *msg, unsigned msg_len )
{
  struct sockaddr_in broadcast_addr = {};
  broadcast_addr.sin_family      = AF_INET;
  broadcast_addr.sin_addr.s_addr = inet_addr(ip_host);
  broadcast_addr.sin_port        = htons(ip_port);
  return sendto(sockfd, msg, msg_len, 0, (sockaddr*)&broadcast_addr, sizeof(broadcast_addr));
}


/*******************************************************************************
 *  recv_msg
 *  params:
 *    sockfd:  int             => socket file descriptor
 *    msg:     char*           => A buffer to store the received message
 *    msg_len: unsigned*       => The length of the received message
 *    ip_host: char*           => IPv4 Broadcast Address that sent the message
 *    ip_port: unsigned short* => Port the message was sent on
 *  returns:
 *    The number of bytes received
 ******************************************************************************/
extern "C"
int recv_msg( int sockfd, char *msg, unsigned *msg_len, char *ip_host, unsigned short *ip_port )
{
  struct sockaddr_in recv_addr = {};
  unsigned addr_len = sizeof(recv_addr);
  unsigned bytes_recv;
  bytes_recv = recvfrom(sockfd, msg, MAX_BUFFSIZE, 0, (sockaddr*)&recv_addr, &addr_len);
  *msg_len = bytes_recv;
  std::string recv_host = std::string( inet_ntoa(recv_addr.sin_addr) );
  std::memcpy(ip_host, recv_host.c_str(), recv_host.size());
  *ip_port = ntohs(recv_addr.sin_port);
  return bytes_recv;
}


