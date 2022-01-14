import { createContext } from 'react';
import IssueSearchStore from './store';
import { IssueSearchProps } from './index';

export interface IIssueSearchContext extends Omit<IssueSearchProps, 'onChange'> {
  store: IssueSearchStore
  projectId?: string
}

const context = createContext<IIssueSearchContext>({} as IIssueSearchContext);
export default context;
