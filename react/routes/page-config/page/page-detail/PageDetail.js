import React, {
  useState, useEffect, useContext,
} from 'react';
import { Link } from 'react-router-dom';
import { observer } from 'mobx-react-lite';
import { Spin, Checkbox, Breadcrumb as Bread } from 'choerodon-ui';
import {
  Page, Content, Breadcrumb,
} from '@choerodon/boot';
import SortTable from '../components/SortTable';
import Store from '../stores';
import PageConfigStore from '../../stores';
import './PageDetail.less';

const { Item } = Bread;

/** 
 * 函数组件
 * 使用HOOKS相关内容
 */
function PageDetail(props) {
  const context = useContext(Store);
  const contextPageConfig = useContext(PageConfigStore);
  const [height, setHeight] = useState(0);
  const {
    pageStore, AppState,
  } = context;
  const {
    name, type, id, organizationId: orgId, category,
  } = AppState.currentMenuType;
  const {
    pageDetailItem,
  } = contextPageConfig;
  const [loading, setLoading] = useState(false);
  // 设置高度
  const initCurrentMenuType = () => {
    pageStore.initCurrentMenuType(AppState.currentMenuType);
  };

  const loadPageDetail = () => {
    setLoading(true);
    pageStore.loadPageDetail(pageDetailItem.pageCode).then(() => {
      setLoading(false);
    });
  };

  // componentDidMount
  useEffect(() => {
    initCurrentMenuType();
    loadPageDetail();
  }, []);
  // componentWillUnmount
  useEffect(() => () => {
    pageStore.setPage([]);
  }, []);


  const handleDrag = (result, postData) => {
    const page = pageStore.getPageDetail;
    const { name: n } = page;
    pageStore.updateFieldOrder(pageDetailItem.pageCode, postData).then((data) => {
      if (data) {
        pageStore.setPageDetail({
          name: n,
          content: result.map((item) => {
            if (data.fieldId === item.fieldId) {
              return {
                ...item,
                objectVersionNumber: data.objectVersionNumber,
                display: data.display,
              };
            } else {
              return item;
            }
          }),
        });
      }
    });
  };

  const onDisplayChange = (item) => {
    if (item.system && item.required) {
      return;
    }
    const field = {
      display: !item.display,
      objectVersionNumber: item.objectVersionNumber,
    };
    pageStore.updateField(item.fieldId, pageDetailItem.pageCode, field);
  };

  const getColume = () => [
    {
      title: '字段',
      dataIndex: 'fieldName',
      width: '25%',
    },
    {
      title: '显示范围',
      dataIndex: 'contextName',
      width: '25%',
    },
    {
      title: '字段类型',
      dataIndex: 'fieldTypeName',
      width: '25%',
    },
    {
      title: '显示',
      dataIndex: 'display',
      width: '15',
      render: (display, record) => (
        <div>
          <Checkbox
            checked={record.display}
            disabled={record.system && record.required}
            onChange={() => onDisplayChange(record)}
          />
        </div>
      ),
    },
  ];
  const page = pageStore.getPageDetail;
  const { name: pageName, content = [] } = page;
  return (
    <Page
      className="c7n-page-detail"
      service={AppState.currentMenuType.type === 'project' ? [
        'choerodon.code.project.setting.page.ps.scheme',
      ] : [
        'choerodon.code.organization.setting.issue.page.ps.scheme',
      ]}
    >
      <Breadcrumb custom>
        <Item>{name}</Item>
        <Item>
          <Link to={`/agile/page/scheme?type=${type}&id=${id}&name=${name}&category=${category}&organizationId=${orgId}&orgId=${orgId}`}>页面</Link>
        </Item>
        <Item>{pageName}</Item>
      </Breadcrumb>
      <Content>
        <Spin spinning={loading}>
          <SortTable
            pagination={false}
            columns={getColume()}
            dataSource={content.slice()}
            filterBar={false}
            handleDrag={handleDrag}
            hight={height}
          />
        </Spin>
      </Content>
    </Page>

  );
}

export default observer(PageDetail);
