#include "udp_interface.hpp"
#include <cstring>
#include <string>
#include <iostream>

#define MAX_BUFFSIZE 4096

extern "C"
int get_socket( void )
{
  return socket(AF_INET, SOCK_DGRAM, 0);
}

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

extern "C"
int send_msg( int sockfd, char *ip_host, short ip_port, char *msg, unsigned msg_len )
{
  struct sockaddr_in send_addr = {};
  send_addr.sin_family      = AF_INET;
  send_addr.sin_addr.s_addr = inet_addr(ip_host);
  send_addr.sin_port        = htons(ip_port);
  return sendto(sockfd, msg, msg_len, 0, (sockaddr*)&send_addr, sizeof(send_addr));
}

extern "C"
int broadcast_msg( int sockfd, char *ip_host, unsigned short ip_port, char *msg, unsigned msg_len )
{
  struct sockaddr_in broadcast_addr = {};
  broadcast_addr.sin_family      = AF_INET;
  broadcast_addr.sin_addr.s_addr = inet_addr(ip_host);
  broadcast_addr.sin_port        = htons(ip_port);
  return sendto(sockfd, msg, msg_len, 0, (sockaddr*)&broadcast_addr, sizeof(broadcast_addr));
}

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

