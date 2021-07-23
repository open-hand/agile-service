import { IModalProps, IRole } from '@/common/types';
import React, {
  useMemo, useCallback, useEffect, useState,
  useRef,
} from 'react';
import {
  DataSet, Form, Select, Modal, SelectBox,
} from 'choerodon-ui/pro';
import { ShowHelp } from 'choerodon-ui/pro/lib/field/enum';
import SelectUser, { SelectUserProps } from '@/components/select/select-user';
import {
  commonApi, IPageFieldCreatePermissionItem, IPageFieldPermissionItem, pageConfigApi, userApi,
} from '@/api';
import { groupBy, isEmpty, uniq } from 'lodash';
import UserTag from '@/components/tag/user-tag';
import { observer, useComputed } from 'mobx-react-lite';
import styles from './index.less';
import { mainValueTransformUserAndRole } from './utils';

interface Props {
  data?: IPageFieldPermissionItem[]
  onOk?: Function
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
  const ref = useRef<any>(null);
  const showSelectUser = useComputed(() => {
    const selectedRoles = record?.get(`${name}_Roles`);
    return selectedRoles?.includes('specificUser');
  }, [record?.get(`${name}_Roles`)]);

  return (
    <SelectUser
      // @ts-ignore
      ref={ref as any}
      name={name}
      style={{ width: '100%' }}
      multiple
      request={() => userApi.getAllInProject(undefined, 1, undefined, 0)}
      trigger={['click'] as any}
      afterLoad={(users) => {
        users.splice(0, 0, ...(roles.map((item) => ({ ...item, id: `role-${item.id}` }))) as any);
        return users;
      }}
      optionRenderer={(item: any) => (item.isRole ? item.name : <UserTag data={item} size={14} />)}
      popupContent={(
        <div role="none" onClick={(e) => e.stopPropagation()} onMouseDown={(e) => e.stopPropagation()} className={styles.select_content}>
          <SelectBox name={`${name}_Roles`} vertical getPopupContainer={(node) => node.parentNode as any} multiple />
          {true && (
            <SelectUser
              name={`${name}_UserIds`}
              hidden={!showSelectUser}
              request={() => userApi.getAllInProject(undefined, 1, undefined, 0)}
              style={{ marginTop: '.2rem', width: '100%' }}
              getPopupContainer={(node) => node.parentNode as any}
            />
          )}
        </div>

      )}
      onChange={(value) => {
        console.log('change....', value);
        const { userIds: newUserIds, roles: newRoles } = mainValueTransformUserAndRole(value);

        record?.init({ [`${name}_UserIds`]: newUserIds, [`${name}_Roles`]: newRoles });

        // record?.init({ [`${name}_UserIds`]: [], [`${name}_Roles`]: [] });
      }}
      {...otherProps}
    />
  );
});
const RoleConfigModal: React.FC<{ modal?: IModalProps } & Props> = observer(({
  modal, data, fields, issueTypeId, onOk,
}) => {
  const [roles, setRoles] = useState<IRoleWithSelectOption[]>();
  useEffect(() => {
    commonApi.getRoles().then((res) => {
      const newRoles = res.map((item) => ({
        ...item, value: item.id, meaning: item.name, realName: item.name, isRole: true,
      }));
      newRoles.push({ id: 'specificUser', value: 'specificUser', meaning: '指定人' } as any);
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
        name: 'edit_UserIds', label: '指定用户', multiple: true,
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
      { name: 'onlyView_UserIds', label: '指定用户', multiple: true },
      {
        name: 'onlyView_Roles',
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
        if (!isEmpty(additionalName)) {
          console.log('name :>> ', name, value, record?.getPristineValue(name));
        } else {
          return;
        }
        const currentRoles = record.get(`${mainName}_Roles`).filter((item: any) => item !== 'specificUser')
          .map((item: any) => `role-${item}`);
        console.log('currentRoles...', currentRoles);
        if (additionalName.includes('UserIds')) {
          record.init(mainName, [...currentRoles, ...value]);
        } else if (additionalName.includes('Roles')) {
          const currentUserIds: any[] = record.get(`${mainName}_Roles`).includes('specificUser') ? record.get(`${mainName}_UserIds`) : [];
          currentUserIds.length === 0 && record.set(`${mainName}_UserIds`, undefined);
          record.init(mainName, [...currentRoles, ...currentUserIds]);
        }
      },
    },
  }), [roles]);

  const handleSubmit = useCallback(async () => {
    const { edit = [], onlyView = [] } = dataset.current?.toData();
    const { userIds: editUserIds, roles: editRoles } = mainValueTransformUserAndRole(edit, true);
    const { userIds: onlyViewUserIds, roles: onlyViewRoles } = mainValueTransformUserAndRole(onlyView, true);

    const writePermission: IPageFieldCreatePermissionItem = { scope: 'write', userIds: editUserIds, roleIds: editRoles };
    const readPermission: IPageFieldCreatePermissionItem = {
      scope: 'read',
      userIds: [...editUserIds, ...onlyViewUserIds],
      roleIds: [...editRoles, ...onlyViewRoles],
    };
    console.log('writePermission :>> ', writePermission);
    console.log('readPermission :>> ', readPermission);
    // return false;
    await pageConfigApi.createPermission({
      fields,
      permissions: [writePermission, readPermission],
      issueTypeIds: [issueTypeId],
    });
    onOk && onOk();
    return true;
  }, [dataset, fields, issueTypeId]);
  useEffect(() => {
    if (data) {
      const edit: string[] = [];
      const onlyView: { userIds: string[], roles: string[], mainValues: string[] } = { userIds: [], roles: [], mainValues: [] };

      data.forEach((item) => {
        if (item.scope === 'write') {
          // edit.push(...item.userIds);
          // onlyView.userIds.push(...item.userIds);
          // onlyView.push(...item.userIds);
        } else if (item.scope === 'read') {
          onlyView.userIds.push(...(item.userIds || []));
          onlyView.roles.push(...(item.roleIds || []));
          onlyView.mainValues.push(...onlyView.roles.map((i) => `role-${i}`));
          onlyView.mainValues.push(...onlyView.userIds);
          onlyView.userIds.length > 0 && onlyView.roles.push('specificUser');

          // onlyView.push(...item.userIds);
        }
      });
      dataset.current?.init({ onlyView: onlyView.mainValues, onlyView_UserIds: onlyView.userIds, onlyView_Roles: onlyView.roles });
    }
  }, [data, dataset]);
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
        {roles ? <SelectUserWithRole name="onlyView" record={dataset.current} roles={roles} /> : <Select name="edit" />}

        {/* <SelectUser
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
        /> */}
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
