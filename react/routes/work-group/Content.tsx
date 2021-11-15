import React, { useCallback, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Page, Breadcrumb, Content, Header, HeaderButtons,
} from '@choerodon/boot';
import { isEmpty, map } from 'lodash';
import { Modal } from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import ResizeContainer from '@/components/resize-divider';
import Styles from './index.less';
import { useWorkGroupStore } from '@/routes/work-group/stores';
import GroupTree from '@/routes/work-group/components/group-tree';
import UserTable from '@/routes/work-group/components/user-table';
import { workGroupApi } from '@/api/WorkGroup';
import openAddUser from '@/routes/work-group/components/add-user';

const { Divider, Section } = ResizeContainer;
const removeModalKey = Modal.key();

const WorkGroupContent = observer(() => {
  const {
    mainStore,
    MAX_WIDTH,
    MIN_WIDTH,
    tableDs,
    NOT_ASSIGN_ID,
    ROOT_ID,
  } = useWorkGroupStore();

  const refresh = useCallback(() => {
    mainStore.loadTreeData();
    tableDs.query();
  }, []);

  const isDisabled = useMemo(() => {
    const { id } = mainStore.getSelectedMenu || {};
    return !id || [NOT_ASSIGN_ID, ROOT_ID].includes(id);
  }, [mainStore.getSelectedMenu]);

  const handleAddMember = useCallback(() => {
    const { id } = mainStore.getSelectedMenu || {};
    openAddUser({ workGroupId: id, refresh });
  }, [mainStore.getSelectedMenu]);

  const handleRemoveMember = useCallback(async () => {
    try {
      const userIds = map(tableDs.selected, (record: Record) => record.get('id'));
      if (!isEmpty(userIds)) {
        const { id } = mainStore.getSelectedMenu || {};
        await workGroupApi.removeUserByGroup(id, userIds);
        tableDs.unSelectAll();
        refresh();
      }
      return true;
    } catch (e) {
      return false;
    }
  }, [tableDs.selected, mainStore.getSelectedMenu]);

  const openRemoveModal = useCallback(() => {
    Modal.open({
      title: '移除成员',
      key: removeModalKey,
      children: '确认将成员移出当前工作组？',
      onOk: handleRemoveMember,
    });
  }, [handleRemoveMember]);

  return (
    <Page className={Styles.pageWrap}>
      <Header>
        <HeaderButtons
          items={[{
            display: true,
            name: '加入成员',
            icon: 'add',
            handler: handleAddMember,
            disabled: isDisabled,
          }, {
            display: true,
            name: '移除成员',
            icon: 'exit_to_app',
            handler: openRemoveModal,
            disabled: isDisabled || isEmpty(tableDs.selected),
          }, {
            display: true,
            icon: 'refresh',
            handler: refresh,
          }]}
        />
      </Header>
      <Breadcrumb />
      <Content className={Styles.content}>
        <ResizeContainer style={{ height: '100%' }}>
          <Section
            size={{
              width: MIN_WIDTH,
              minWidth: MIN_WIDTH,
              maxWidth: MAX_WIDTH,
            }}
            style={{
              minWidth: MIN_WIDTH,
              maxWidth: MAX_WIDTH,
            }}
          >
            <GroupTree />
          </Section>
          <Divider />
          <Section
            style={{ flex: 1 }}
            size={{
              width: 'auto',
            }}
          >
            <div className={Styles.table}>
              <UserTable />
            </div>
          </Section>
        </ResizeContainer>
      </Content>
    </Page>
  );
});

export default WorkGroupContent;
