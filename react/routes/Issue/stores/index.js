import React, {
  createContext, useMemo, useEffect, useState, 
} from 'react';
import { set } from 'mobx';
import { DataSet } from 'choerodon-ui/pro';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import { getFoundationHeader } from '@/api/NewIssueApi';
import IssueDataSet from './IssueDataSet';

const Store = createContext();

export default Store;

export const StoreProvider = inject('AppState')(injectIntl(
  (props) => {
    const { intl, children, AppState: { currentMenuType: { id: projectId, organizationId }, userInfo: { id: userId } } } = props;   
    // const intlPrefix = 'global.saga';
    const [fields, setFields] = useState([]);
    useEffect(() => {
      const loadData = async () => {
        const Fields = await getFoundationHeader();
        setFields(Fields);
      };
      loadData();
    }, []);
    
    const dataSet = useMemo(() => new DataSet(IssueDataSet({
      intl, projectId, organizationId,
    })), []);
    // 初始化issue总数，不然之后会不更新
    set(dataSet, { issueTotal: 10 });
    set(dataSet, { issuePageSize: 10 });
    set(dataSet, { issueCurrentPage: 1 });
    /**
    * detail data
    * 详情页数据
    * @param id
    */
    
    const value = {
      ...props,
      fields,
      dataSet,
      projectId, 
      organizationId,
      userId,
      prefixCls: 'c7n-issue',
      // intlPrefix,
    };
    return (
      <Store.Provider value={value}>
        {children}
      </Store.Provider>
    );
  },
));
