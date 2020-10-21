import { createContext, useContext } from 'react';

interface Context {
  scale: number
}
const ChartContext = createContext({
  scale: 1,
} as Context);

function useChartContext() {
  const context = useContext(ChartContext);
  return context;
}

function useFontSize() {
  const { scale } = useChartContext();
  return (size: number) => scale * size;
}
export { useChartContext, useFontSize };
export default ChartContext;
