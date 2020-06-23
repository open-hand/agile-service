import {
  useContext, createContext, 
} from 'react';
import { priorityApi } from '@/api';

const PriorityContext = createContext({
  data: [],
  refresh: priorityApi.loadByProject.bind(priorityApi),
});
export { PriorityContext };

export default function usePriorities() {
  const { data, refresh } = useContext(PriorityContext);  
  return [data, refresh];
}
