import React, {
  createContext, useState, useContext, useMemo, useEffect,
} from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import { DataSet } from 'choerodon-ui/pro/lib';
import { useDetail } from '@/components/detail-container';
import PublishVersionDataSet from './PublishVersionDataSet';
import store, { PublishDetailStore } from './store';

interface Context {
  tableDataSet: DataSet
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

    const tableDataSet = useMemo(() => new DataSet(PublishVersionDataSet()), []);
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
    };
    return (
      <PublishVersionContext.Provider value={value}>
        {props.children}
      </PublishVersionContext.Provider>
    );
  },
));
export default PublishVersionProvider;
