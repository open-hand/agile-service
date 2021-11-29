import React, {
  createContext, useContext, useMemo,
} from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { getMenuType } from '@/utils/common';
import SortTableDataSet from './SortTableDataSet';
import AddUnselectedDataSet from './AddUnselectedDataSet';
import PageIssueTypeStore from './PageIssueTypeStore';
import { usePageConfigContext } from '../../stores';

interface Context {
  sortTableDataSet: DataSet,
  addUnselectedDataSet: DataSet,
  pageIssueTypeStore: PageIssueTypeStore,
  isInProgram: boolean,
  isProject: boolean,
  prefixCls: 'c7n-agile-page-config-page-issue-type',
}

const PageIssueTypeContext = createContext({} as Context);

export function usePageIssueTypeStore() {
  return useContext(PageIssueTypeContext);
}

const PageIssueTypeProvider = observer(
  (props: any) => {
    const sortTableDataSet = useMemo(() => new DataSet(SortTableDataSet()), []);
    const { isInProgram } = usePageConfigContext();
    const addUnselectedDataSet = useMemo(() => new DataSet(AddUnselectedDataSet()), []);
    const pageIssueTypeStore = useMemo(() => new PageIssueTypeStore(
      { sortTableDataSet, addUnselectedDataSet, isInProgram },
    ), [sortTableDataSet, addUnselectedDataSet, isInProgram]);
    const value = {
      ...props,
      sortTableDataSet,
      addUnselectedDataSet,
      pageIssueTypeStore,
      prefixCls: 'c7n-agile-page-config-page-issue-type',
      isProject: getMenuType() === 'project',
    };
    return (
      <PageIssueTypeContext.Provider value={value}>
        {props.children}
      </PageIssueTypeContext.Provider>
    );
  },
);
export default PageIssueTypeProvider;
