import React, { useEffect, useContext } from 'react';
import { observer } from 'mobx-react-lite';
import {
  TabPage as Page, Header, Content, Action, Permission, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, Icon, Table, Tooltip, Modal,
} from 'choerodon-ui/pro';
import ClickText from '@/components/ClickText';

import CreateEditForm from './create-edit-form';
import DeleteModal from './delete-modal';
import useLinkHomeStore from './stores';


import './IssueLinkHome.less';

const { Column } = Table;

const modelKey = Modal.key();
const deleteKey = Modal.key();

const modelStyle = {
  width: 380,
};

function IssueLinkHome() {
  const {
    formatMessage,
    type, id, orgId,
    issueLinkTableDs,
  } = useLinkHomeStore();


  function openCreateEditModal(isEdit) {
    const record = isEdit ? issueLinkTableDs.current : issueLinkTableDs.create();
    const titleId = !isEdit ? 'issue_link.create.issue_link' : 'issue_link.edit.issue_link';
    const okTextId = !isEdit ? 'create' : 'save';
    const values = {
      record,
    };

    Modal.open({
      key: modelKey,
      title: formatMessage({ id: titleId }),
      drawer: true,
      children: <CreateEditForm {...values} />,
      style: modelStyle,
      okText: formatMessage({ id: okTextId }),
      cancelText: formatMessage({ id: 'cancel' }),
      onOk: async () => issueLinkTableDs.submit().then((res) => {
        issueLinkTableDs.query();
        return !!res;
      }),      
      afterClose: () => {
        issueLinkTableDs.reset();
      },
    });
  }

  function openDeleteModal() {
    const record = issueLinkTableDs.current;
    const values = {
      id,
      linkTypeId: record.get('linkTypeId'),
      formatMessage,
      handleRefresh: () => {
        issueLinkTableDs.query();
      },
    };

    Modal.open({
      key: deleteKey,
      title: formatMessage({ id: 'issue_link.delete.only' }, { name: `: ${record.get('linkName')}` }),
      children: <DeleteModal {...values} />,
      cancelText: formatMessage({ id: 'cancel' }),
      okText: formatMessage({ id: 'delete' }),
      okProps: { color: 'red' },
      cancelProps: { color: 'dark' },
    });
  }


  function renderLinkName({ text }) {
    return (
      <ClickText
        value={text}
        onClick={openCreateEditModal.bind(null, true)}
        clickAble
        showToolTip
        permissionCode={['agile-service.issue-link-type.updateIssueLinkType']}
      />
    );
  }

  function renderAction() {
    const actionData = [
      {
        service: ['agile-service.issue-link-type.deleteIssueLinkType'],
        text: formatMessage({ id: 'delete' }),
        action: openDeleteModal,
      },
    ];

    return <Action data={actionData} />;
  }

  function renderDesc({ text }) {
    return (
      <div style={{ width: '100%', overflow: 'hidden' }}>
        <Tooltip placement="topLeft" mouseEnterDelay={0.5} title={text}>
          <p
            style={{
              overflow: 'hidden',
              textOverflow: 'ellipsis',
              whiteSpace: 'nowrap',
              marginBottom: 0,
              lineHeight: '24px',
            }}
          >
            {text}
          </p>
        </Tooltip>
      </div>
    );
  }
  return (
    <Page
      service={[
        'choerodon.code.project.setting.issue.ps.issuelink',
      ]}
      className="c7n-issue-link"
    >
      <Header title={formatMessage({ id: 'issue.link_task' })}>
        <Permission
          service={['choerodon.code.project.setting.issue.ps.createfastsearch']}
        >
          <Button funcType="flat" onClick={openCreateEditModal.bind(null, false)}>
            <Icon type="playlist_add icon" />
            <span>{formatMessage({ id: 'issue_link.create' })}</span>
          </Button>
        </Permission>
      </Header>
      <Breadcrumb />
      <Content>
        <Table dataSet={issueLinkTableDs} pristine>
          <Column name="linkName" renderer={renderLinkName} />
          <Column renderer={renderAction} width="0.7rem" />
          <Column name="outWard" renderer={renderDesc} />
          <Column name="inWard" renderer={renderDesc} />
        </Table>
      </Content>
    </Page>
  );
}

export default observer(IssueLinkHome);
