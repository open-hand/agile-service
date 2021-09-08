import React from 'react';
import { observer } from 'mobx-react-lite';
import {
  TabPage as Page, Header, Content, Action, Breadcrumb,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import {
  Table, Modal,
} from 'choerodon-ui/pro';

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
      onOk: async () => {
        if (await issueLinkTableDs.validate()) {
          await issueLinkTableDs.submit();
          issueLinkTableDs.query();
          return true;
        }
        return false;
      },
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
      issueCount: record.get('issueCount'),
      name: record.get('linkName'),
      formatMessage,
      handleRefresh: () => {
        issueLinkTableDs.query();
      },
    };

    Modal.open({
      key: deleteKey,
      title: formatMessage({ id: 'issue_link.delete.only' }),
      children: <DeleteModal {...values} />,
      cancelText: formatMessage({ id: 'cancel' }),
      okText: formatMessage({ id: 'delete' }),
      className: 'c7n-deleteLink-modal',
    });
  }

  function renderAction() {
    const actionData = [
      {
        text: formatMessage({ id: 'edit' }),
        action: () => openCreateEditModal(true),
      },
      {
        service: ['choerodon.code.project.setting.issue.ps.deletelink'],
        text: formatMessage({ id: 'delete' }),
        action: openDeleteModal,
      },
    ];

    return <Action data={actionData} className="c7n-issue-link-action" />;
  }

  return (
    <Page
      service={[
        'choerodon.code.project.setting.issue.ps.issuelink',
        'choerodon.code.project.setting.issue.ps.updatelink',
        'choerodon.code.project.setting.issue.ps.deletelink',
      ]}
      className="c7n-issue-link"
    >
      <Header title={formatMessage({ id: 'issue.link_task' })}>
        <HeaderButtons items={[{
          name: formatMessage({ id: 'issue_link.create' }),
          icon: 'playlist_add',
          handler: () => openCreateEditModal(false),
          display: true,
          permissions: ['choerodon.code.project.setting.issue.ps.createfastsearch'],
        }]}
        />
      </Header>
      <Breadcrumb />
      <Content>
        <Table dataSet={issueLinkTableDs} pristine>
          <Column name="linkName" />
          <Column renderer={renderAction} width="0.7rem" />
          <Column name="outWard" />
          <Column name="inWard" />
        </Table>
      </Content>
    </Page>
  );
}

export default observer(IssueLinkHome);
