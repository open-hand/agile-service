import React, {
  createContext, useState, useContext, useMemo, useEffect,
} from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import { DataSet } from 'choerodon-ui/pro/lib';
import { useDetail } from '@/components/detail-container';
import PublishVersionDataSet from './PublishVersionDataSet';
import store, { PublishDetailStore } from './store';
import IssueInfoTableDataSet from './IssueInfoTableDataSet';
import IssueDiffDataSet from './IssueDiffDataSet';

interface Context {
  tableDataSet: DataSet
  issueInfoTableDataSet:DataSet
  issueDiffDataSet:DataSet
  detailProps: any
  store: PublishDetailStore
  prefixCls: string
}
const PublishVersionContext = createContext({} as Context);
export function usePublishVersionContext() {
  return useContext(PublishVersionContext);
}
const PublishVersionProvider = injectIntl(inject('AppState')(
  (props: any) => {
    const [detailProps] = useDetail();
    const issueDiffDataSet = useMemo(() => new DataSet(IssueDiffDataSet()), []);
    const issueInfoTableDataSet = useMemo(() => new DataSet(IssueInfoTableDataSet(store.getCurrentData.id)), [store.getCurrentData.id]);
    const tableDataSet = useMemo(() => new DataSet(PublishVersionDataSet()), []);
    useEffect(() => {
      if (store.getCurrentData.id) {
        issueInfoTableDataSet.query();
      }
    }, [store.getCurrentData.id]);
    useEffect(() => {
      async function init() {
        await tableDataSet.query().then((res) => {
          if (res.list[0]) {
            store.select(res.list[0]);
          }
        });
        tableDataSet.select(0);
      }
      init();
    }, [tableDataSet]);
    const value = {
      ...props,
      store,
      detailProps,
      prefixCls: 'c7n-agile-publish-version',
      tableDataSet,
      issueInfoTableDataSet,
      issueDiffDataSet,
    };
    return (
      <PublishVersionContext.Provider value={value}>
        {props.children}
      </PublishVersionContext.Provider>
    );
  },
));
export default PublishVersionProvider;
