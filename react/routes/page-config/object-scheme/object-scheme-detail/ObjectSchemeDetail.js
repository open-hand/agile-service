/* eslint-disable no-nested-ternary */
import React, {
  Fragment, useState, useEffect, useContext,
} from 'react';
import { observer } from 'mobx-react-lite';
import { Link } from 'react-router-dom';
import {
  Table, Spin, Button, Checkbox, Modal, Tag, Menu, Icon,
  Breadcrumb as Bread,
} from 'choerodon-ui';
import {
  TabPage as Page, Header, Content, Breadcrumb, Choerodon,
} from '@choerodon/boot';
import TypeTag from '../../../../components/TypeTag';
import Store from '../stores';
import PageConfigStore from '../../stores';
import ObjectSchemeField from '../object-scheme-field';
import CreateField from '../components/create-field';
import './ObjectSchemeDetail.less';
import TableDropMenu from '../../../../common/TableDropMenu';

const { confirm } = Modal;
const { Item } = Bread;
const showIcons = {
  史诗: {
    icon: 'agile_epic',
    colour: '#743be7',
    typeCode: 'issue_epic',
    name: '史诗',
  },
  故事: {
    icon: 'agile_story',
    colour: '#00bfa5',
    typeCode: 'story',
    name: '故事',
  },
  特性: {
    icon: 'agile-feature',
    colour: '#3D5AFE',
    typeCode: 'feature',
    name: '特性',
  },
  缺陷: {
    icon: 'agile_fault',
    colour: '#f44336',
    typeCode: 'bug',
    name: '缺陷',
  },
  任务: {
    icon: 'agile_task',
    colour: '#4d90fe',
    typeCode: 'task',
    name: '任务',
  },
  子任务: {
    icon: 'agile_subtask',
    colour: '#4d90fe',
    typeCode: 'sub_task',
    name: '子任务',
  },
};


function ObjectSchemeDetail(props) {
  const context = useContext(Store);
  const contextPageConfig = useContext(PageConfigStore);
  const { AppState, objectSchemeStore, prefixCls } = context;
  const [loading, setLoading] = useState(false);
  const [updateVisible, setUpdateVisible] = useState(false);
  const [detailRecord, setDetailRecord] = useState({});
  const [addVisible, setAddVisible] = useState(false);

  // const [scrollHeight, setScrollHeight] = useState(3000);

  const initCurrentMenuType = () => {
    objectSchemeStore.initCurrentMenuType(AppState.currentMenuType);
  };

  const loadScheme = () => {
    const { objectDetailItem } = contextPageConfig;
    setLoading(true);
    objectSchemeStore.loadSchemeDetail(objectDetailItem.schemeCode).then(() => {
      setLoading(false);
    });
  };
  const onRequiredChange = (item) => {
    if (item.system) {
      return;
    }
    if (!item.required && !item.defaultValue) {
      Choerodon.prompt('必填字段请设置默认值！');
    }
    const field = {
      required: !item.required,
      objectVersionNumber: item.objectVersionNumber,
    };
    objectSchemeStore.updateField(item.id, field);
  };

  // 在当前页弹出，不渲染
  const editField = (item) => {
    const newItem = item;
    setDetailRecord(newItem);
    setUpdateVisible(true);
  };

  const deleteField = (item) => {
    objectSchemeStore.deleteField(item.id).then(() => {
      loadScheme();
    });
  };

  const handleDelete = (item) => {
    if (item.system) {
      return;
    }
    // const that = this;
    confirm({
      title: '删除字段',
      content: (
        <div>
          <p style={{ marginBottom: 10 }}>
            <div style={{ marginBottom: 10 }}>{`删除自定义字段：${item.name}`}</div>
            <div>注意：将会从所有使用的问题中删除此字段，并且字段数据会清空。你确定要删除此字段吗？</div>
          </p>
        </div>
      ),
      onOk() {
        deleteField(item);
      },
      onCancel() { },
      okText: '删除',
      okType: 'danger',
      width: 512,
    });
  };


  const onClose = () => {
    setUpdateVisible(false);
  };
  const onCloseAdd = () => {
    setAddVisible(false);
  };
  const onOk = () => {
    loadScheme();
    setUpdateVisible(false);
  };

  const onOkAdd = () => {
    loadScheme();
    setAddVisible(false);
  };
  const renderDropDown = (text, record) => {
    if (record.system) {
      return text;
    }
    const menu = (
      <Menu onClick={() => handleDelete(record)}>
        <Menu.Item key="del">
          {/* <Tooltip placement="top" title="删除">
            <Button shape="circle" size="small" onClick={() => handleDelete(record)}>
              <i className="icon icon-delete" />
            </Button>
          </Tooltip> */}
          <span>删除</span>
        </Menu.Item>
      </Menu>

    );
    return (
      <TableDropMenu
        menu={menu}
        onClickEdit={editField.bind(this, record)}
        text={text}
        isHasMenu={!(record.system || (AppState.currentMenuType.type === 'project' && !record.projectId))}
      />

    );
  };
  const getColume = () => [
    {
      title: '字段',
      dataIndex: 'name',
      width: '15%',
      render: (text, record) => renderDropDown(text, record),

    },
    {
      title: '显示范围',
      dataIndex: 'contextName',
      width: '25%',
      render: contextName => (
        <Fragment>
          {contextName.split(',').map(name => (
            showIcons[name] ? <div><TypeTag data={showIcons[name]} showName /></div> : name
          ))}
        </Fragment>
      ),
    },
    {
      title: '字段来源',
      width: '15%',
      render: ({ projectId, system }) => (
        <div>
          {system
            ? <Tag style={{ color: 'rgba(0,0,0,0.65)', borderColor: '#d9d9d9', background: '#fafafa' }}>系统</Tag>
            : projectId
              ? <Tag color="orange">项目</Tag>
              : <Tag color="geekblue">组织</Tag>}
        </div>

      ),
    },
    {
      title: '字段类型',
      dataIndex: 'fieldTypeName',
      width: '25%',
    },
    {
      title: '必填项',
      dataIndex: 'required',
      width: 15,
      render: (required, record) => (
        <div>
          <Checkbox
            checked={record.required}
            disabled={record.system || (AppState.currentMenuType.type === 'project' && !record.projectId)}
            onChange={() => onRequiredChange(record)}
          />
        </div>
      ),
    },

  ];


  useEffect(() => {
    initCurrentMenuType();
    loadScheme();
  }, []);

  useEffect(() => () => {
    objectSchemeStore.setSchemeDetail({
      content: [],
    });
  }, []);

  useEffect(() => {
    // setScrollHeight(document.getElementsByClassName('page-content') && document.getElementsByClassName('page-content')[0].clientHeight - 70);
  }, []);

  const render = () => {
    const menu = AppState.currentMenuType;
    const {
      name, type, id, organizationId: orgId,
    } = menu;
    const scheme = objectSchemeStore.getSchemeDetail;
    const { content = [] } = scheme;
    const { objectDetailItem } = contextPageConfig;
    return (
      <Page
        service={AppState.currentMenuType.type === 'project' ? [
          'agile-service.project-object-scheme-field.listQuery',
          'agile-service.project-object-scheme-field.create',
          'agile-service.project-object-scheme-field.checkCode',
          'agile-service.project-object-scheme-field.checkName',
          'agile-service.project-object-scheme-field.listQuery',
          'agile-service.project-object-scheme-field.queryById',
          'agile-service.project-object-scheme-field.update',
          'agile-service.project-object-scheme-field.delete',
        ] : [
          'agile-service.object-scheme-field.listQuery',
          'agile-service.object-scheme-field.create',
          'agile-service.object-scheme-field.checkCode',
          'agile-service.object-scheme-field.checkName',
          'agile-service.object-scheme-field.listQuery',
          'agile-service.object-scheme-field.queryById',
          'agile-service.object-scheme-field.update',
          'agile-service.object-scheme-field.delete',
        ]}
      >
        <Header>
          <Button
            funcType="flat"
            onClick={() => {
              setAddVisible(true);
            }}
          >
            <Icon type="playlist_add icon" />
            <span>创建字段</span>
          </Button>
        </Header>
        <Breadcrumb />
        <Content className={`${prefixCls}-detail-content`}>
          <Spin spinning={loading}>
            <Table
              pagination={false}
              rowKey={record => record.id}
              columns={getColume()}
              dataSource={content}
              filterBar={false}
              // scroll={{ y: scrollHeight }}
            />
          </Spin>
          {updateVisible
            ? (
              <ObjectSchemeField
                store={objectSchemeStore}
                onClose={onClose}
                onOk={onOk}
                visible={updateVisible}
                fieldId={detailRecord.id}
              />
            )
            : null
          }
          {addVisible
            ? (
              <div>
                <CreateField
                  store={objectSchemeStore}
                  onClose={onCloseAdd}
                  onOk={onOkAdd}
                  visible={addVisible}
                  schemeCode={objectDetailItem.schemeCode}
                  {...props}
                />
              </div>
            )
            : null
          }
        </Content>
      </Page>
    );
  };
  return render();
}

export default observer(ObjectSchemeDetail);
