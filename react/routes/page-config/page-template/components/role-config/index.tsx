import { IModalProps } from '@/common/types';
import React, { useMemo, useCallback, useEffect } from 'react';
import {
  DataSet, Form, Select, Modal,
} from 'choerodon-ui/pro';
import { ShowHelp } from 'choerodon-ui/pro/lib/field/enum';
import SelectUser from '@/components/select/select-user';
import { IPageFieldCreatePermissionItem, IPageFieldPermissionItem, pageConfigApi } from '@/api';
import { uniq } from 'lodash';

interface Props {
  data?: IPageFieldPermissionItem[]
  fields: any,
  issueTypeId: string
}
const RoleConfigModal: React.FC<{ modal?: IModalProps } & Props> = ({
  modal, data, fields, issueTypeId,
}) => {
  const dataset = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      { name: 'edit', label: '可编辑', multiple: true },
      { name: 'onlyView', label: '仅查看', multiple: true },
    ],
  }), []);
  const handleSubmit = useCallback(async () => {
    const { edit = [], onlyView } = dataset.toJSONData()[0] as any;
    const writePermission: IPageFieldCreatePermissionItem = { scope: 'write', userIds: uniq([...edit]) };
    const readPermission: IPageFieldCreatePermissionItem = { scope: 'read', userIds: uniq([...edit, ...onlyView]) };
    console.log('writePermission :>> ', writePermission);
    console.log('readPermission :>> ', readPermission);

    pageConfigApi.createPermission({
      fields,
      permissions: [writePermission, readPermission],
      issueTypeIds: [issueTypeId],
    });
    return true;
  }, [dataset, fields, issueTypeId]);
  useEffect(() => {
    if (data) {
      const edit: string[] = [];
      const onlyView: string[] = [];

      data.forEach((item) => {
        if (item.scope === 'write') {
          edit.push(...item.userIds);
          onlyView.push(...item.userIds);
        } else if (item.scope === 'read') {
          onlyView.push(...item.userIds);
        }
      });
      dataset.current?.init({ edit, onlyView });
    }
  }, [data, dataset]);
  useEffect(() => { modal?.handleOk(handleSubmit); });
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
        <SelectUser
          name="onlyView"
        />
      </Form>
    </div>
  );
};
function openPageRoleConfigModal(props: Props) {
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
