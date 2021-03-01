import React, {
  createContext, useMemo, useEffect, useState, useRef,
} from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import { fieldApi, permissionApi } from '@/api';
import IssueStore, { getSystemFields } from '@/stores/project/issue/IssueStore';
import { useIssueSearchStore } from '@/components/issue-search';
import IssueDataSet from '@/components/issue-table/dataSet';
import useQueryString from '@/hooks/useQueryString';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { transformFilter, handleSelect, handleUnSelect } from './utils';

const Store = createContext();

export default Store;

export const StoreProvider = inject('AppState')(injectIntl(
  (props) => {
    const { intl, children, AppState: { currentMenuType: { id: projectId, organizationId }, userInfo: { id: userId } } } = props;
    const [fields, setFields] = useState([]);
    const [hasBatchDeletePermission, setHasBatchDeletePermission] = useState(false);
    const permissionRef = useRef(false);
    permissionRef.current = hasBatchDeletePermission;
    const params = useQueryString();
    const [tableListMode, changeTableListMode] = useState(() => {
      if (params.tableListMode) {
        return params.tableListMode;
      }
      const defaultMode = localPageCacheStore.getItem('issues.table.mode');
      if (defaultMode === 'list') {
        return defaultMode;
      }
      return undefined;
    });/** 类型为boolean 时 则为用户操作  类型为string 即值为list时为缓存数据 */
    useEffect(() => {
      if (typeof (tableListMode) === 'boolean') {
        tableListMode ? localPageCacheStore.setItem('issues.table.mode', 'list') : localPageCacheStore.remove('issues.table.mode');
        handleUnSelect({ dataSet }, issueSearchStore, hasBatchDeletePermission);
        IssueStore.query();
      }
    }, [tableListMode]);
    useEffect(() => {
      const loadData = async () => {
        const Fields = await fieldApi.getFoundationHeader();
        setFields(Fields);
      };
      loadData();
    }, []);

    useEffect(() => {
      const getBatchDeletePermission = async () => {
        const res = await permissionApi.check(['choerodon.code.project.cooperation.work-list.ps.issue.batchDelete']);
        setHasBatchDeletePermission(res[0] && res[0].approve);
      };
      getBatchDeletePermission();
    }, []);

    const issueSearchStore = useIssueSearchStore({
      getSystemFields,
      transformFilter,
      defaultChosenFields: Array.isArray(localPageCacheStore.getItem('issues')) ? new Map(localPageCacheStore.getItem('issues').map((item) => [item.code, item])) : undefined,
    });
    const dataSet = useMemo(() => new DataSet(IssueDataSet({
      intl,
      projectId,
      organizationId,
      issueSearchStore,
      IssueStore,
      tableListMode,
      events: {
        select: () => handleSelect({ dataSet }, issueSearchStore, permissionRef.current),
        selectAll: () => handleSelect({ dataSet }, issueSearchStore, permissionRef.current),
        unSelect: handleUnSelect,
        unSelectAll: handleUnSelect,
        load: () => {
          // 有筛选，自动展开
          if (issueSearchStore.isHasFilter && IssueStore.tableRef.current) {
            IssueStore.tableRef.current.tableStore.expandAll();
          }
        },
      },
    })), [intl, issueSearchStore, organizationId, projectId, tableListMode]);
    IssueStore.dataSet = dataSet;
    /**
    * detail data
    * 详情页数据
    * @param id
    */

    const value = {
      ...props,
      tableListMode,
      changeTableListMode,
      issueSearchStore,
      fields,
      dataSet,
      projectId,
      organizationId,
      userId,
      prefixCls: 'c7n-issue',
    };
    return (
      <Store.Provider value={value}>
        {children}
      </Store.Provider>
    );
  },
));
