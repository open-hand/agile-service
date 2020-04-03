import {
  useContext, createContext, 
} from 'react';
import { loadPriorities } from '@/api/NewIssueApi';

const PriorityContext = createContext({
  data: [],
  refresh: loadPriorities,
});
export { PriorityContext };

export default function usePriorities() {
  const { data, refresh } = useContext(PriorityContext);  
  return [data, refresh];
}
