import {
  useContext, createContext, 
} from 'react';
import { getDefaultPriority } from '@/api/NewIssueApi';

const DefaultPriorityContext = createContext({
  data: null,
  refresh: getDefaultPriority,
});
export { DefaultPriorityContext };

export default function useDefaultPriority() {
  const { data, refresh } = useContext(DefaultPriorityContext);  
  return [data, refresh];
}
