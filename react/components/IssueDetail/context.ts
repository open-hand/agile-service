import { createContext } from 'react';
import { IssueDetailStore } from './store';

interface Context {
  store: IssueDetailStore
}
const IssueDetailContext = createContext({} as Context);
export default IssueDetailContext;
