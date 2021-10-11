import React, { useContext } from 'react';
import { Link, withRouter } from 'react-router-dom';
import { Button, Tooltip, Icon } from 'choerodon-ui';
import { Modal, Table } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  TabPage as Page, Header, Content, stores, Breadcrumb,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import './ComponentHome.less';
import TableDropMenu from '@/components/table-drop-menu';
import UserTag from '@/components/tag/user-tag';
import DeleteComponent from '../ComponentComponent/DeleteComponent';
import Store from './stores';
import openComponentModal from '../ComponentComponent/CreateModal';

const { AppState } = stores;
const deleteKey = Modal.key();
const { Column } = Table;

function ComponentHome() {
  const { dataSet, history } = useContext(Store);

  const menu = AppState.currentMenuType;
  const urlParams = AppState.currentMenuType;
  const {
    type, id, organizationId: orgId,
  } = menu;
  const handleOk = () => {
    dataSet.query();
  };

  const openDeleteModal = (component) => {
    Modal.open({
      key: deleteKey,
      title: '删除模块',
      style: {
        width: 520,
      },
      className: 'c7n-component-deleteModal',
      children: (
        <DeleteComponent
          component={component}
          onOk={handleOk}
          history={history}
        />
      ),
    });
  };

  const renderMenu = (text, record) => (
    <TableDropMenu
      menuData={[
        {
          action: () => { openComponentModal({ componentId: record?.get('componentId'), name: record?.get('name'), onOk: handleOk }); },
          text: '编辑',
        },
        {
          key: 'delete',
          text: '删除',
          service: ['choerodon.code.project.setting.issue.ps.deletecomponent'],
        }]}
      onMenuClick={() => openDeleteModal(record)}
      tooltip={false}
      text={(
        <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={text}>
          <p
            style={{
              overflow: 'hidden',
              textOverflow: 'ellipsis',
              whiteSpace: 'nowrap',
              marginBottom: 0,
              lineHeight: '32px',
            }}
          >
            {text}
          </p>
        </Tooltip>
      )}
      permissionType={type}
      permission={{ projectId: id, organizationId: orgId }}
    />
  );
  const renderTable = () => (
    <Table
      dataSet={dataSet}
    >
      <Column
        name="name"
        width={250}
        renderer={({ text, record }) => renderMenu(text, record)}
      />
      <Column
        name="issueCount"
        className="c7n-agile-table-cell-click"
        renderer={({ text: issueCount, record }) => (
          issueCount ? (
            <Link
              to={`/agile/work-list/issue?type=${urlParams.type}&id=${urlParams.id}&name=${encodeURIComponent(urlParams.name)}&organizationId=${urlParams.organizationId}&orgId=${urlParams.organizationId}&paramType=component&paramId=${encodeURIComponent(record.get('componentId'))}&paramName=${encodeURIComponent(`模块"${record.get('name')}"下的工作项`)}`}
            >
              {issueCount}
              {'issues'}
            </Link>
          ) : null
        )}
      />
      <Column
        name="manager"
        className="c7n-agile-table-cell"
        renderer={({ record }) => (
          record.get('managerId') ? (
            <UserTag
              style={{ display: 'inline-flex' }}
              data={{
                id: record.get('managerId'),
                loginName: record.get('managerLoginName'),
                realName: record.get('managerRealName'),
                tooltip: record.get('managerName'),
                imageUrl: record.get('imageUrl'),
              }}
            />
          ) : null
        )}
      />
      <Column name="description" className="c7n-agile-table-cell" />
      <Column name="defaultAssigneeRole" className="c7n-agile-table-cell" />
      <Column name="sequence" className="c7n-agile-table-cell" />
    </Table>
  );
  return (
    <Page
      className="c7n-component"
      service={[
        'choerodon.code.project.setting.issue.ps.component',
      ]}
    >
      <Header title="模块管理">
        <HeaderButtons items={[{
          name: '创建模块',
          icon: 'playlist_add',
          handler: () => { openComponentModal({ onOk: handleOk }); },
          display: true,
          permissions: ['choerodon.code.project.setting.issue.ps.createcomponent'],
        }]}
        />
      </Header>
      <Breadcrumb />
      <Content>
        {renderTable()}
      </Content>
    </Page>
  );
}

export default withRouter(observer(ComponentHome));
