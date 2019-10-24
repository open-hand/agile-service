import React, { useState, useEffect, useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { Table, Spin } from 'choerodon-ui';
import {
  TabPage as Page, Content, Breadcrumb,
} from '@choerodon/boot';

import Store from '../stores';
import PageConfigStore from '../../stores';
import './ObjectSchemeHome.less';

function ObjectSchemeHome(props) {
  const context = useContext(Store);
  const contextPageConfig = useContext(PageConfigStore);
  // const { AppState, pageStore } = props.value;
  const { AppState, objectSchemeStore, prefixCls } = context;
  const objectScheme = objectSchemeStore.getObjectScheme;
  const [loading, setLoading] = useState(false);
  const [filterName, setFilterName] = useState('');
  const [pagination, setPagination] = useState({
    current: 1,
    pageSize: 10,
    total: undefined,
  });

  const initCurrentMenuType = () => {
    objectSchemeStore.initCurrentMenuType(AppState.currentMenuType);
  };

  const loadAllObjectScheme = () => {
    setLoading(true);
    objectSchemeStore.loadObjectScheme({ param: filterName }).then(() => {
      setLoading(false);
    });
  };
  // 点击时 设定状态， 不进行跳转页面，在本页面进行渲染 ObjectSchemeDetail 组件
  const showDetail = (item) => {
    const { setObjectDetailItem, setAddObVisible } = contextPageConfig;
    setAddObVisible(true);
    setObjectDetailItem(item);
  };
  const renderName = (text, record) => (
    <div className="">
      <a role="button" onClick={showDetail.bind(this, record)} onKeyDown={null}>{text}</a>
    </div>
  );

  const getColume = () => [
    {
      title: '方案名称',
      dataIndex: 'name',
      width: '45%',
      render: (text, record) => renderName(text, record),
    },
    {
      title: '方案类型',
      dataIndex: 'schemeCodeName',
      width: '45%',
    },
  ];

  const handleTableChange = (newPagination, filters, sorter, barFilters) => {
    setPagination(newPagination);
    setFilterName(filters);
  };

  useEffect(() => {
    initCurrentMenuType();
    loadAllObjectScheme();
  }, []);


  return (
    <Page className={`${prefixCls}-home`}>
      {/* <Breadcrumb title="字段列表" /> */}
      <Breadcrumb title="" />

      <Content className={`${prefixCls}-home-content`}>
        <Spin spinning={loading}>
          <Table
            pagination={false}
            rowKey={record => record.id}
            columns={getColume()}
            dataSource={objectScheme}
            filterBarPlaceholder="过滤表"
            onChange={handleTableChange}
          />
        </Spin>
      </Content>
    </Page>
  );
}

export default observer(ObjectSchemeHome);
