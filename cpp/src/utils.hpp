#include <chrono>
#include <random>

inline float random_float()
{
  std::default_random_engine generator;
  std::normal_distribution<float> distribution(0.5,1.0);
  return distribution(generator);
}

inline unsigned float_to_useconds(float seconds)
{
  std::chrono::duration<float> f_seconds;
  std::chrono::microseconds   useconds;
  f_seconds = std::chrono::duration<float>(seconds);
  useconds = std::chrono::duration_cast<std::chrono::microseconds>(f_seconds);
  return useconds.count();
}
