import React, {
  createContext, useState, useContext, useMemo, useEffect, useCallback,
} from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import { DataSet } from 'choerodon-ui/pro/lib';
import { useDetail } from '@/components/detail-container';
import { getProjectId, getOrganizationId } from '@/utils/common';
import { IPublishVersionData } from '@/api';
import { set } from 'lodash';
import PublishVersionDataSet from './PublishVersionDataSet';
import { PublishDetailStore } from './store';
import IssueInfoTableDataSet from './IssueInfoTableDataSet';
import IssueDiffDataSet from './IssueDiffDataSet';
import { IPublishVersionProps } from '..';

export interface IPublishVersionContext extends IPublishVersionProps<IPublishVersionData> {
  tableDataSet: DataSet
  issueInfoTableDataSet: DataSet
  issueDiffDataSet: DataSet
  detailProps: any
  store: PublishDetailStore
  preview: boolean
  prefixCls: string
}
const PublishVersionContext = createContext({} as IPublishVersionContext);
export function usePublishVersionContext() {
  return useContext(PublishVersionContext);
}
const PublishVersionProvider = injectIntl(inject('AppState')(
  (props: IPublishVersionProps<IPublishVersionData> & { [propsName: string]: any }) => {
    const [detailProps] = useDetail();
    const { leftListDataSetConfig, publishVersionId, pageContentEmpty } = props;
    const { open } = detailProps;
    const store = useMemo(() => props.store || new PublishDetailStore(), [props.store]);
    const issueDiffDataSet = useMemo(() => new DataSet(IssueDiffDataSet()), []);
    const issueInfoTableDataSet = useMemo(() => new DataSet(IssueInfoTableDataSet()), []);
    const tableDataSet = useMemo(() => new DataSet({ ...PublishVersionDataSet(), ...leftListDataSetConfig }), [leftListDataSetConfig]);
    const handleSelectIssue = useCallback((id: string) => open({
      path: 'issue',
      props: {
        issueId: id,
        projectId: getProjectId(),
        organizationId: getOrganizationId(),
      },
      events: {
        update: () => {
          issueInfoTableDataSet.query(issueInfoTableDataSet.currentPage);
          // issueDiffDataSet.query(issueDiffDataSet.currentPage)
          // handleRefresh();
        },
      },
    }), [issueInfoTableDataSet, open]);
    useEffect(() => { store.clear(); }, [store]);
    useEffect(() => {
      store.init({ selectIssue: handleSelectIssue }, { detailProps });
    }, [detailProps, handleSelectIssue, store]);
    useEffect(() => {
      async function init() {
        await tableDataSet.query().then((res) => {
          if (res?.list[0]) {
            store.select(res.list[0]);
          }
        });
        tableDataSet.select(0);
      }
      init();
    }, [publishVersionId, store, tableDataSet]);
    const value = {
      ...props,
      preview: false,
      store: store as PublishDetailStore,
      detailProps,
      prefixCls: 'c7n-agile-publish-version',
      tableDataSet,
      issueInfoTableDataSet,
      issueDiffDataSet,
    };
    typeof (pageContentEmpty) === 'function' && set(value, 'pageContentEmpty', pageContentEmpty(value));
    return (
      <PublishVersionContext.Provider value={value}>
        {props.children}
      </PublishVersionContext.Provider>
    );
  },
));
export default PublishVersionProvider;
