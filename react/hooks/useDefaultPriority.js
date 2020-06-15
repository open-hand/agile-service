import {
  useContext, createContext, 
} from 'react';
import { priorityApi } from '@/api';

const DefaultPriorityContext = createContext({
  data: null,
  refresh: priorityApi.getDefaultByProject.bind(priorityApi),
});
export { DefaultPriorityContext };

export default function useDefaultPriority() {
  const { data, refresh } = useContext(DefaultPriorityContext);  
  return [data, refresh];
}
