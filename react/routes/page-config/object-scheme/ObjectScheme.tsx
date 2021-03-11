import React from 'react';
import { observer } from 'mobx-react-lite';
import {
  Table, Button, Modal, Menu, Icon,
} from 'choerodon-ui/pro';
import { Tag } from 'choerodon-ui';
import {
  TabPage as Page, Header, Content, Breadcrumb, Choerodon,
} from '@choerodon/boot';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { TableQueryBarType } from 'choerodon-ui/pro/lib/table/enum';
import { pageConfigApi } from '@/api/PageConfig';
import { getMenuType } from '@/utils/common';
import NewCheckBox from '@/components/check-box';
import { useObjectSchemeStore } from './stores';
import TableDropMenu from '../../../common/TableDropMenu';
import CreateField from '../components/create-field';
import RequiredPrompt from './components/required-prompt';
import './ObjectScheme.less';
import { disabledEditDefaultFields, orgDisabledEditDefaultFields } from '../page-issue-type/components/sort-table/useTextEditToggle';
import { openSyncDefaultValueEditForm } from './components/sync-default-value-modal';
import openImportField from './components/import-field';

const { Column } = Table;
enum IRequireScopeType {
  all = 'ALL',
  part = 'PART',
  none = 'NONE',
}

const createModelKey = Modal.key();
const editModelKey = Modal.key();

const createModelStyle = {
  width: 740,
};

function ObjectScheme() {
  const {
    prefixCls, schemeTableDataSet, intl: { formatMessage },
    schemeCode,
  } = useObjectSchemeStore();
  function handleRefresh() {
    schemeTableDataSet.query();
  }

  function handleRemove() {
    const record = schemeTableDataSet.current;
    const modalProps = {
      title: formatMessage({ id: 'field.delete.title' }, { name: record?.get('name') }),
      children: formatMessage({ id: 'field.delete.msg' }),
      okText: formatMessage({ id: 'delete' }),
      okProps: { color: 'red' },
      cancelProps: { color: 'dark' },
    };
    schemeTableDataSet.delete(record, modalProps);
  }
  function handleSyncDefault() {
    const record = schemeTableDataSet.current!.clone();
    openSyncDefaultValueEditForm(record, prefixCls);
  }
  function handleClickMenu({ key }: { key: string }) {
    if (key === 'del') {
      handleRemove();
    }
    if (key === 'sync') {
      handleSyncDefault();
    }
  }
  function handleCheckChange(value: boolean) {
    if (handleContinueCheckChange()) {
      return value;
    }
    return !value;
  }
  function handleContinueCheckChange(secondEntry = false) {
    const record = schemeTableDataSet.current;
    const defaultValue = record?.get('defaultValue');
    const requiredScope = record?.get('requiredScope');
    const required = requiredScope !== IRequireScopeType.all;
    if (record?.get('system')) {
      return false;
    }
    if (required && !defaultValue) {
      Choerodon.prompt(formatMessage({ id: 'field.required.msg' }));
      return false;
    }
    if (secondEntry || !openPromptForRequire(record?.get('name'), required)) {
      pageConfigApi.updateRequired(record?.get('id'), required).then(() => {
        handleRefresh();
      });
      return true;
    }
    return false;
  }
  function openPromptForRequire(fieldName: string, required: boolean) {
    const isOpen: boolean = !(localStorage.getItem('agile.page.field.setting.required.prompt') === 'false');
    if (isOpen) {
      const promptText = `确定要将【${fieldName}】设置为${!required ? '不' : ''}必填项吗？
      设置后${getMenuType() !== 'project' ? '组织下所有' : ''}项目中该字段都将为${!required ? '不' : ''}必填，这将会影响【快速创建】问题的使用。`;
      Modal.open({
        key: Modal.key(),
        className: `${prefixCls}-detail-prompt`,
        title: `确认设置为${!required ? '不' : ''}必填`,
        children: (<RequiredPrompt
          formatMessage={formatMessage}
          onContinue={handleContinueCheckChange}
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
    const disabledFields = getMenuType() === 'project' ? disabledEditDefaultFields : orgDisabledEditDefaultFields;
    // 系统字段 和项目层的组织字段 禁止编辑,禁止删除
    const disabledEditDel = system || (getMenuType() === 'project' && projectId === null);
    const menuItems = [
      <Menu.Item key="sync">
        <span>{formatMessage({ id: 'defaultValue.sync' })}</span>
      </Menu.Item>,
    ];
    if (!disabledEditDel) {
      menuItems.push(
        <Menu.Item key="del">
          <span>{formatMessage({ id: 'delete' })}</span>
        </Menu.Item>,
      );
    }
    const menu = (
      <Menu onClick={handleClickMenu}>
        {menuItems}
      </Menu>
    );

    return (
      <div className="c7n-table-cell-drop-menu">
        <TableDropMenu
          menu={menu}
          tooltip
          onClickEdit={disabledEditDel ? undefined : openEditFieldModal}
          text={text}
          isHasMenu={!(system && disabledFields.includes(record?.get('code')))}
        />
      </div>
    );
  };

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

  const renderRequired = ({ record, name }: RenderProps) => {
    const system = record?.get('system');
    const requiredScope = record?.get('requiredScope');
    const projectId = record?.get('projectId');
    return (
      <div>
        <NewCheckBox
          defaultChecked={requiredScope === 'ALL'}
          indeterminate={requiredScope === 'PART'}
          checked={requiredScope === 'ALL'}
          record={record}
          name={name!}
          disabled={system || (getMenuType() === 'project' && !projectId)}
          onChange={handleCheckChange}
        />
      </div>
    );
  };

  const service = getMenuType() === 'project' ? [
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
        <Button
          funcType={'flat' as FuncType}
          onClick={openImportField}
        >
          <Icon type="archive" />
          <span>{formatMessage({ id: 'field.import' })}</span>
        </Button>
      </Header>
      <Breadcrumb />
      <Content className={`${prefixCls}-detail-content`}>
        <Table dataSet={schemeTableDataSet} queryBar={'none' as TableQueryBarType} className={`${prefixCls}-detail-content-table`}>
          <Column name="name" renderer={renderDropDown} />
          <Column name="contextName" width={245} />
          <Column name="fieldOrigin" renderer={renderFieldOrigin} header={formatMessage({ id: 'field.origin' })} />
          <Column name="fieldTypeName" />
          <Column name="required" renderer={renderRequired} />
        </Table>
      </Content>
    </Page>
  );
}

export default observer(ObjectScheme);
