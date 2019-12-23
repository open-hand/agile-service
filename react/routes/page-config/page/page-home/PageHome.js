import React, { useState, useEffect, useContext } from 'react';
import { Link } from 'react-router-dom';
import { observer } from 'mobx-react-lite';
import {
  Table, Spin, Breadcrumb as Bread,
} from 'choerodon-ui';
import {
  TabPage as Page, Content, Breadcrumb,
} from '@choerodon/boot';
import Store from '../stores';
import PageConfigStore from '../../stores';
import './PageHome.less';

const { Item } = Bread;

function PageHome(props) {
  const context = useContext(Store);
  const contextPageConfig = useContext(PageConfigStore);
  const { AppState, pageStore, prefixCls } = context;
  const menu = AppState.currentMenuType;
  const {
    name, type, id, organizationId: orgId, 
  } = menu;
  const page = pageStore.getPage;
  const [loading, setLoading] = useState(false);
  const [filterName, setFilterName] = useState('');
  const [pagination, setPagination] = useState({
    current: 1,
    pageSize: 10,
    total: undefined,
  });

  const initCurrentMenuType = () => {
    pageStore.initCurrentMenuType(AppState.currentMenuType);
  };

  
  const loadPage = (newPage, size) => {
    setLoading(true);

    pageStore.loadPage(newPage, size, { param: filterName }).then(() => {
      setLoading(false);
    });
  };

  const loadAllPage = () => {
    setLoading(true);
    pageStore.loadPage({ param: filterName }).then(() => {
      setLoading(false);
    });
  };


  const showDetail = (item) => {
    const { setPageDetailVisible, setPageDetailItem } = contextPageConfig;
    // const urlParams = AppState.currentMenuType;
    setPageDetailVisible(true);
    setPageDetailItem(item);
  };

  const renderName = (text, record) => (
    // <Tooltip placement="top" title="详情">
    <div className="">
      <a role="button" onClick={showDetail.bind(this, record)} onKeyDown={null}>{text}</a>
    </div>
    /* </Tooltip> */
  );
  const getColume = () => [
    {
      title: '页面名称',
      dataIndex: 'name',
      width: '45%',
      render: (text, record) => renderName(text, record),
    },
    {
      title: '关联字段方案',
      dataIndex: 'schemeName',
      width: '45%',
    },
  ];

  const handleTableChange = (newPagination, filters, sorter, barFilters) => {
    setPagination(newPagination);
    setFilterName(filters);
  };
  useEffect(() => {
    initCurrentMenuType();
    loadAllPage();
  }, []);

  return (
    <Page 
      className={`${prefixCls}-home`}
      service={AppState.currentMenuType.type === 'project' ? [
        'agile-service.project-page.pageQuery',
      ] : [
        'agile-service.page.pageQuery',
      ]}
    >
      <Breadcrumb />
      <Content className={`${prefixCls}-home-content`}>
        <Spin spinning={loading}>
          <Table
            pagination={false}
            rowKey={record => record.id}
            columns={getColume()}
            dataSource={page}
            filterBar={false}
            scroll={{ x: true }}
            onChange={handleTableChange}
          />
        </Spin>
      </Content>
    </Page>
  );
}

export default observer(PageHome);
