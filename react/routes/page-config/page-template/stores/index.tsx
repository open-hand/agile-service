import React, {
  createContext, useContext, useMemo,
} from 'react';
import { injectIntl, InjectedIntl } from 'react-intl';
import { getMenuType } from '@/utils/common';
import { DataSet } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import SortTableDataSet from './SortTableDataSet';
import AddUnselectedDataSet from './AddUnselectedDataSet';
import PageTemplateStore from './PageTemplateStore';
import { usePageConfigContext } from '../../stores';

interface Context {
  sortTableDataSet: DataSet,
  addUnselectedDataSet: DataSet,
  pageTemplateStore: PageTemplateStore,
  isInProgram: boolean,
  isProject: boolean,
  intl: InjectedIntl,
  prefixCls: 'c7n-agile-page-config-page-issue-type',
}

const PageTemplateContext = createContext({} as Context);

export function usePageTemplateStore() {
  return useContext(PageTemplateContext);
}

const PageTemplateProvider = injectIntl(observer(
  (props: any) => {
    const sortTableDataSet = useMemo(() => new DataSet(SortTableDataSet()), []);
    const { isInProgram } = usePageConfigContext();
    const addUnselectedDataSet = useMemo(() => new DataSet(AddUnselectedDataSet()), []);
    const pageTemplateStore = useMemo(() => new PageTemplateStore(
      { sortTableDataSet, addUnselectedDataSet, isInProgram },
    ), [sortTableDataSet, addUnselectedDataSet, isInProgram]);
    const value = {
      ...props,
      sortTableDataSet,
      addUnselectedDataSet,
      pageTemplateStore,
      prefixCls: 'c7n-agile-page-config-page-issue-type',
      isProject: getMenuType() === 'project',
    };
    return (
      <PageTemplateContext.Provider value={value}>
        {props.children}
      </PageTemplateContext.Provider>
    );
  },
));
export default PageTemplateProvider;
