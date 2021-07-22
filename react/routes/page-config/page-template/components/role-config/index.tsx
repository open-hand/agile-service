import { IModalProps } from '@/common/types';
import React, { useMemo } from 'react';
import {
  DataSet, Form, Select, Modal,
} from 'choerodon-ui/pro';
import { ShowHelp } from 'choerodon-ui/pro/lib/field/enum';
import SelectUser from '@/components/select/select-user';

interface Props {
    data?: any

}
const RoleConfigModal: React.FC<{ modal?: IModalProps } & Props> = () => {
  const dataset = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      { name: 'edit', label: '可编辑' },
      { name: 'onlyView', label: '仅查看' },
    ],
  }), []);
  return (
    <div>
      <span style={{ display: 'inline-block', marginBottom: 16 }}>设置哪些成员或角色可以查看或编辑当前字段</span>
      <Form dataSet={dataset}>
        <SelectUser
          name="edit"
          showHelp={'tooltip' as ShowHelp}
          help="有编辑权限则一定有查看权限"
          afterLoad={(users) => {
            users.splice(0, 0, ...[{ id: 'owner', realName: '项目所有者' }, { id: 'member', realName: '项目成员' }] as any[]);
            return users;
          }}
        />
        <Select name="onlyView" />
      </Form>
    </div>
  );
};
function openPageRoleConfigModal(props?: Props) {
  Modal.open({
    key: Modal.key(),
    title: '权限配置',
    style: {
      width: 340,
    },
    drawer: true,
    children: <RoleConfigModal {...props} />,
  });
}
export default openPageRoleConfigModal;
