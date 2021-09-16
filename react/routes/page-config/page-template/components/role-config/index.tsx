import React, {
  useMemo, useCallback, useEffect, useState,
} from 'react';
import {
  DataSet, Form, Select, Modal, SelectBox,
} from 'choerodon-ui/pro';
import {
  isEmpty, noop,
} from 'lodash';
import { observer, useComputed } from 'mobx-react-lite';
import { usePersistFn } from 'ahooks';
import SelectUser, { SelectUserProps } from '@/components/select/select-user';
import {
  commonApi, getProjectUsersByIds, IPageFieldCreatePermissionItem, IPageFieldPermissionItem, pageConfigApi,
} from '@/api';
import UserTag from '@/components/tag/user-tag';
import { IModalProps, IRole, User } from '@/common/types';
import styles from './index.less';
import { mainValueTransformUserAndRole } from './utils';

interface Props {
  data?: IPageFieldPermissionItem[]
  onOk?: () => void
  fields: any,
  issueTypeId: string
}
interface IRoleWithSelectOption extends IRole {
  meaning: string
  value: string
}
const SelectUserWithRole: React.FC<SelectUserProps & { roles: IRoleWithSelectOption[] }> = observer(({
  name, record, roles, ...otherProps
}) => {
  const rolesMap = useMemo(() => new Map(roles.map((item) => ([`role-${item.id}`, item]))), []);
  const showSelectUser = useComputed(() => {
    const selectedRoles = record?.get(`${name}_Roles`);
    return selectedRoles?.includes('specificUser');
  }, [record?.get(`${name}_Roles`)]);

  return (
    <Select
      name={name}
      style={{ width: '100%' }}
      multiple
      trigger={['click'] as any}
      renderer={({ record: r, value: item }) => (typeof (item) === 'string' ? rolesMap.get(item)?.meaning : <UserTag data={item} size={14} />)}
      popupContent={(
        <div role="none" onClick={(e) => e.stopPropagation()} onMouseDown={(e) => e.stopPropagation()} className={styles.select_content}>
          <SelectBox name={`${name}_Roles`} vertical multiple />
          {true && (
            <SelectUser
              name={`${name}_Users`}
              hidden={!showSelectUser}
              selectedUser={record?.getState(`${name}_defaultSelectUsers`)}
              style={{ marginTop: '.2rem', width: '100%' }}
              getPopupContainer={(node) => node.parentNode as any}
            />
          )}
        </div>

      )}
      onChange={(value) => {
        const { users: newUsers, roles: newRoles } = mainValueTransformUserAndRole(value);
        record?.init({ [`${name}_Users`]: newUsers, [`${name}_Roles`]: newRoles });
      }}
      {...otherProps}
    />
  );
});
const RoleConfigModal: React.FC<{ modal?: IModalProps } & Props> = observer(({
  modal, data, fields, issueTypeId, onOk: propsOnOk,
}) => {
  const [roles, setRoles] = useState<IRoleWithSelectOption[]>();
  useEffect(() => {
    commonApi.getRoles().then((res) => {
      const newRoles = res.map((item) => ({
        ...item, value: item.id, meaning: item.name, realName: item.name, isRole: true,
      }));
      newRoles.push({ id: 'specificUser', value: 'specificUser', meaning: '指定用户' } as any);
      setRoles(newRoles);
    });
  }, []);
  const dataset = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'edit', label: '可编辑', multiple: true,
      },
      {
        name: 'edit_Users', label: '指定用户', type: 'object' as any, multiple: true, textField: 'realName', valueField: 'id',
      },
      {
        name: 'edit_Roles',
        label: '',
        multiple: true,
        options: new DataSet({
          paging: false,
          autoQuery: false,
          autoCreate: false,
          data: roles,
        }),
      },
      { name: 'onlyView', label: '可见', multiple: true },
      {
        name: 'onlyView_Users', label: '指定用户', textField: 'realName', valueField: 'id', type: 'object' as any, multiple: true,
      },
      {
        name: 'onlyView_Roles',
        type: 'object' as any,
        label: '',
        multiple: true,
        options: new DataSet({
          paging: false,
          autoQuery: false,
          autoCreate: false,
          data: roles,
        }),
      },

    ],
    events: {
      update: ({ value, name, record }: any) => {
        const [mainName, additionalName] = String(name).split('_');
        if (isEmpty(additionalName)) {
          return;
        }
        const currentRoles = record.get(`${mainName}_Roles`).filter((item: any) => item !== 'specificUser')
          .map((item: any) => `role-${item}`);
        if (additionalName.includes('Users')) {
          record.set(mainName, [...currentRoles, ...value]);
        } else if (additionalName.includes('Roles')) {
          const currentUsers: any[] = record.get(`${mainName}_Roles`).includes('specificUser') ? record.get(`${mainName}_Users`) : [];
          currentUsers.length === 0 && record.init(`${mainName}_Users`, undefined);
          record.set(mainName, [...currentRoles, ...currentUsers]);
        }
      },
    },
  }), [roles]);
  const onOk = usePersistFn(propsOnOk || noop);
  const handleSubmit = useCallback(async () => {
    const { edit = [], onlyView = [] } = dataset.current?.toData();
    const { users: editUsers, roles: editRoles } = mainValueTransformUserAndRole(edit, true);
    const { users: onlyViewUsers, roles: onlyViewRoles } = mainValueTransformUserAndRole(onlyView, true);

    const writePermission: IPageFieldCreatePermissionItem = { scope: 'write', userIds: editUsers.map((i) => i.id), roleIds: editRoles };
    const readPermission: IPageFieldCreatePermissionItem = {
      scope: 'read',
      userIds: [...editUsers, ...onlyViewUsers].map((i) => i.id),
      roleIds: [...editRoles, ...onlyViewRoles],
    };

    await pageConfigApi.createPermission({
      fields,
      permissions: [writePermission, readPermission],
      issueTypeIds: [issueTypeId],
    });
    onOk();
    return true;
  }, [dataset, fields, issueTypeId, onOk]);
  useEffect(() => {
    async function initEditData() {
      const edit: string[] = [];
      const onlyView: { users: User[], userIds: string[], roles: string[], mainValues: Array<string | User> } = {
        users: [], userIds: [], roles: [], mainValues: [],
      };

      data?.forEach((item) => {
        if (item.scope === 'write') {
          // edit.push(...item.users);
          // onlyView.users.push(...item.users);
          // onlyView.push(...item.users);
        } else if (item.scope === 'read') {
          onlyView.userIds.push(...(item.userIds || []));
          onlyView.roles.push(...(item.roleIds || []));
          onlyView.mainValues.push(...onlyView.roles.map((i) => `role-${i}`));
          onlyView.userIds.length > 0 && onlyView.roles.push('specificUser');
        }
      });

      const users = onlyView.userIds.length > 0 ? await getProjectUsersByIds(onlyView.userIds) : [];
      onlyView.users.push(...users);
      onlyView.mainValues.push(...users);
      onlyView.users.length > 0 && onlyView.roles.push('specificUser');

      dataset.current?.setState('onlyView_defaultSelectUsers', users);
      dataset.current?.init({ onlyView: onlyView.mainValues, onlyView_Users: users, onlyView_Roles: onlyView.roles });
    }
    if (data && roles) {
      initEditData();
    }
  }, [data, dataset, roles]);
  useEffect(() => { modal?.handleOk(handleSubmit); });
  return (
    <div>
      <span style={{ display: 'inline-block', marginBottom: 16 }}>设置哪些成员或角色当前字段可见</span>
      <Form dataSet={dataset}>
        {/* {roles ? (
          <SelectUserWithRole
            name="edit"
            record={dataset.current}
            roles={roles}
            showHelp={'tooltip' as ShowHelp}
            help="有编辑权限则一定有查看权限"
          />
        ) : (
          <Select
            name="edit"
            showHelp={'tooltip' as ShowHelp}
            help="有编辑权限则一定有查看权限"
          />
        )} */}
        {roles ? <SelectUserWithRole name="onlyView" record={dataset.current} roles={roles} /> : <Select name="onlyView" />}
      </Form>
    </div>
  );
});
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
