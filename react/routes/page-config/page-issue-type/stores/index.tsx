import React, {
  createContext, useReducer, useContext, useMemo,
} from 'react';
import { injectIntl, InjectedIntl } from 'react-intl';
import { inject } from 'mobx-react';
import { DataSet } from 'choerodon-ui/pro/lib';
import { observer } from 'mobx-react-lite';
import IsInProgramStore from '@/stores/common/program/IsInProgramStore';
import SortTableDataSet from './SortTableDataSet';
import AddUnselectedDataSet from './AddUnselectedDataSet';
import PageIssueTypeStore from './PageIssueTypeStore';

interface Context {
  sortTableDataSet: DataSet,
  addUnselectedDataSet: DataSet,
  pageIssueTypeStore: PageIssueTypeStore,
  isInProgram: boolean,
  intl: InjectedIntl,
}
interface IssueTypeState {
  current: string,
  newCurrent: string,
}
type IssueTypeAction = Required<{ type: string }> & Partial<IssueTypeState>
const PageIssueTypeContext = createContext({} as Context);

export function usePageIssueTypeStore() {
  return useContext(PageIssueTypeContext);
}

const PageIssueTypeProvider = injectIntl(inject('AppState')(observer(
  (props: any) => {
    const sortTableDataSet = useMemo(() => new DataSet(SortTableDataSet()), []);
    const addUnselectedDataSet = useMemo(() => new DataSet(AddUnselectedDataSet()), []);
    const pageIssueTypeStore = useMemo(() => new PageIssueTypeStore(
      { sortTableDataSet, addUnselectedDataSet },
    ), [sortTableDataSet, addUnselectedDataSet]);

    const value = {
      ...props,
      sortTableDataSet,
      addUnselectedDataSet,
      pageIssueTypeStore,
      isInProgram: IsInProgramStore.getIsInProgram,
    };
    return (
      <PageIssueTypeContext.Provider value={value}>
        {props.children}
      </PageIssueTypeContext.Provider>
    );
  },
)));
export default PageIssueTypeProvider;
