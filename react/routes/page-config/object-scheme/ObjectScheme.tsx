import React, {
  Fragment, useContext, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Table, Button, CheckBox, Modal, Menu, Icon,
} from 'choerodon-ui/pro';
import { Tag } from 'choerodon-ui';
import {
  TabPage as Page, Header, Content, Breadcrumb, Choerodon,
} from '@choerodon/boot';

import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { TableQueryBarType } from 'choerodon-ui/pro/lib/table/enum';
import { pageConfigApi } from '@/api/PageConfig';
import Store from './stores';
import TableDropMenu from '../../../common/TableDropMenu';
import TypeTag from '../../../components/TypeTag';
import CreateField from '../components/create-field';
import RequiredPrompt from './components/required-prompt';
import './ObjectScheme.less';

const { Column } = Table;
enum RequireScopeType {
  all = 'ALL',
  part = 'PART',
  none = 'NONE',
}
const showIcons: any = {
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
  需求: {
    icon: 'highlight',
    colour: 'rgba(246,127,90,1)',
    typeCode: 'backlog',
    name: '需求',
  },
};

const createModelKey = Modal.key();
const editModelKey = Modal.key();

const createModelStyle = {
  width: 740,
};

function ObjectScheme() {
  const context = useContext(Store);
  const {
    AppState, prefixCls, schemeTableDataSet,
    intl: { formatMessage },
    schemeCode,
    store,
  } = context;

  function handleRefresh() {
    schemeTableDataSet.query();
  }

  function handleRemove() {
    const record = schemeTableDataSet.current;
    const modalProps = {
      title: formatMessage({ id: 'field.delete.title' }, { name: record.get('name') }),
      children: formatMessage({ id: 'field.delete.msg' }),
      okText: formatMessage({ id: 'delete' }),
      okProps: { color: 'red' },
      cancelProps: { color: 'dark' },
    };
    schemeTableDataSet.delete(record, modalProps);
  }
  function handleCheckChange() {
    const record = schemeTableDataSet.current;
    const defaultValue = schemeTableDataSet.get('defaultValue');
    const requiredScope = record.get('requiredScope');
    const required = requiredScope !== RequireScopeType.all;
    if (record.get('system')) {
      return;
    }
    if (required && defaultValue) {
      Choerodon.prompt(formatMessage({ id: 'field.required.msg' }));
    }
    if (!openPromptForRequire(record.get('name'), required)) {
      pageConfigApi.updateRequired(record.get('id'), required).then(() => {
        handleRefresh();
      });
    }
  }
  function openPromptForRequire(fieldName: string, required: boolean) {
    const isOpen: boolean = !(localStorage.getItem('agile.page.field.setting.required.prompt') === 'false');
    if (isOpen) {
      const promptText = `确定要将【${fieldName}】设置为${!required ? '不' : ''}必填项吗？
      设置后组织下所有项目中该字段都将为${!required ? '不' : ''}必填，这将会影响【快速创建】问题的使用。`;
      Modal.open({
        key: Modal.key(),
        className: `${prefixCls}-detail-prompt`,
        title: '确认设置为必填',
        children: (<RequiredPrompt
          formatMessage={formatMessage}
          onContinue={handleCheckChange}
          promptText={promptText}
        />),
        footer: null,
      });
    }
    return isOpen;
  }
  // 打开创建模态框
  function openCreateFieldModal() {
    const values = {
      formatMessage,
      schemeCode,
      handleRefresh,
    };
    Modal.open({
      key: createModelKey,
      title: formatMessage({ id: 'field.create' }),
      drawer: true,
      children: <CreateField {...values} />,
      style: createModelStyle,
      okText: formatMessage({ id: 'save' }),
      cancelText: formatMessage({ id: 'cancel' }),
    });
  }

  // 打开编辑模态框
  function openEditFieldModal() {
    const record = schemeTableDataSet.current;
    const values = {
      formatMessage,
      schemeCode,
      handleRefresh,
      record,
    };
    Modal.open({
      key: editModelKey,
      title: formatMessage({ id: 'field.edit' }),
      drawer: true,
      children: <CreateField {...values} />,
      style: createModelStyle,
      okText: formatMessage({ id: 'save' }),
      cancelText: formatMessage({ id: 'cancel' }),
    });
  }

  const renderDropDown = ({ text, record }: RenderProps) => {
    const system = record?.get('system');
    const projectId = record?.get('projectId');
    if (system) {
      return text;
    }
    const menu = (
      <Menu onClick={handleRemove}>
        <Menu.Item key="del">
          <span>{formatMessage({ id: 'delete' })}</span>
        </Menu.Item>
      </Menu>

    );
    return (
      <div className="c7n-table-cell-drop-menu">
        <TableDropMenu
          menu={menu}
          onClickEdit={openEditFieldModal}
          text={text}
          isHasMenu={!(system || (AppState.currentMenuType.type === 'project' && !projectId))}
        />
      </div>
    );
  };

  const renderContextName = ({ text }: RenderProps) => (
    <>
      {text?.split(',').map((name: any) => (
        showIcons[name] ? (
          // @ts-ignore
          <TypeTag
            style={{ marginRight: 4 }}
            data={showIcons[name]}
            showName
          />
        ) : name
      ))}
    </>
  );

  const renderFieldOrigin = ({ record }: RenderProps) => {
    const system = record?.get('system');
    const projectId = record?.get('projectId');
    if (system) {
      return <Tag style={{ color: 'rgba(0,0,0,0.65)', borderColor: '#d9d9d9', background: '#fafafa' }}>{formatMessage({ id: 'system' })}</Tag>;
    }
    return projectId
      ? (
        <Tag
          style={{
            background: '#fafafa',
            color: '#5CC2F2',
            borderColor: '#5CC2F2',
          }}
        >
          <span>{formatMessage({ id: 'project' })}</span>
        </Tag>
      )
      : (
        <Tag
          style={{
            background: '#fafafa',
            color: '#3f51b5',
            borderColor: '#3f51b5',
          }}
        >
          <span>{formatMessage({ id: 'organization' })}</span>
        </Tag>
      );
  };

  const renderRequired = ({ record }: RenderProps) => {
    const system = record?.get('system');
    // const required = record?.get('required');
    const requiredScope = record?.get('requiredScope');
    const projectId = record?.get('projectId');
    // console.log('required', required);
    return (
      <div>
        <CheckBox
          defaultChecked={requiredScope === 'ALL'}
          indeterminate={requiredScope === 'PART'}
          checked={requiredScope === 'ALL'}
          disabled={system || (AppState.currentMenuType.type === 'project' && !projectId)}
          onChange={handleCheckChange}
        />
      </div>
    );
  };

  const service = AppState.currentMenuType.type === 'project' ? [
    'choerodon.code.project.setting.page.ps.field',
  ]
    : [
      'choerodon.code.organization.setting.issue.page.ps.filed',
    ];

  return (
    <Page
      service={service}
    >
      <Header>
        <Button
          funcType={'flat' as FuncType}
          onClick={openCreateFieldModal}
        >
          <Icon type="playlist_add icon" />
          <span>{formatMessage({ id: 'field.create' })}</span>
        </Button>
      </Header>
      <Breadcrumb />
      <Content className={`${prefixCls}-detail-content`}>
        <Table dataSet={schemeTableDataSet} queryBar={'none' as TableQueryBarType} className={`${prefixCls}-detail-content-table`}>
          <Column name="name" renderer={renderDropDown} />
          <Column name="contextName" renderer={renderContextName} width={430} />
          <Column name="fieldOrigin" renderer={renderFieldOrigin} header={formatMessage({ id: 'field.origin' })} />
          <Column name="fieldTypeName" />
          <Column name="required" renderer={renderRequired} />
        </Table>
      </Content>
    </Page>
  );
}

export default observer(ObjectScheme);
