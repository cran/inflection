eixf <-
function(XX,FF,f,i)
{
  p=0.5*(XX[i + 1] - XX[i]) * (FF[i] - f(XX[i]) + FF[i + 1] - f(XX[i + 1])) 
  p
}
