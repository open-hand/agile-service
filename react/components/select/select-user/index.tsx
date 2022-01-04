import React, {
  useMemo, forwardRef, useRef, useCallback,
} from 'react';
import { toJS } from 'mobx';
import {
  useCreation, useSafeState,
} from 'ahooks';
import { Select, DataSet } from 'choerodon-ui/pro';
import {
  unionBy, castArray, uniq, includes,
} from 'lodash';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { useComputed } from 'mobx-react-lite';
import classNames from 'classnames';
import { userApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import type { User } from '@/common/types';
import UserTag from '@/components/tag/user-tag';
import Styles from './index.less';
import { refsBindRef, wrapRequestCallback } from '../utils';
import { useNoticeSelectUpdateSelected } from '../useNoticeSelectUpdateSelected';
import { styles as ellipsisStyles } from '../common/utils';

const toArray = (something: any) => (Array.isArray(something) ? something : [something]);
export interface SelectUserProps extends Partial<SelectProps> {
  /** 组织层或项目层 或工作台层 */
  level?: 'org' | 'project' | 'workbench'
  // 由于用户是分页的，有时候已选的用户不在第一页，这时候传id过来，会直接显示id，这里多传一个用户过来，放到options里
  selectedUser?: User | User[],
  selected?: string | string[], /** 需要加载的用户id列表 */
  /** 初始化时确定的额外选项 */
  extraOptions?: {
    id: string,
    realName: string,
  }[],
  dataRef?: React.MutableRefObject<any>
  request?: SelectConfig<User>['request']
  afterLoad?: (users: User[]) => void | User[]
  flat?: boolean
  projectId?: string
  organizationId?: string
  optionRenderer?: (item: User, tooltip?: boolean) => React.ReactElement
  excludeIds?: string[]
}
const SelectUser = forwardRef<Select, SelectUserProps>(({
  selectedUser: propsSelectedUser, extraOptions: propExtraOptions, dataRef, request, level = 'project', afterLoad,
  selected, onOption, flat, projectId, organizationId, optionRenderer, excludeIds, ...otherProps
}, ref: React.Ref<Select>) => {
  const selectRef = useRef<Select>();
  const { selectedUser, extraOptions } = useCreation(() => ({ selectedUser: propsSelectedUser, extraOptions: propExtraOptions }), [propsSelectedUser]);
  const selectDataRef = useRef<DataSet>();
  const requestLoading = useRef<boolean>(true);
  const values = useComputed(() => (selectRef.current?.getValues() || []).flat(Infinity).map((item) => (typeof item === 'object' ? item.id : item)), [selectRef.current?.getValues()]);
  const [forceUpdateValue, setFilterWord] = useNoticeSelectUpdateSelected();
  const selectedUserLoadedIds = useCreation(() => toArray(selectedUser)?.filter((i) => i && typeof (i) === 'object' && i.id).map((i) => i.id), [selectedUser]); // 已经存在的用户查询接口会过滤，避免第二页恰好全是选中的数据，但页面无反应
  const selectedUserIds = useMemo(() => {
    const ids: string[] | string | undefined = toJS(selected);
    const extraOptionIds = extraOptions?.map((i) => i.id) || [];

    // 避免value是对象的情况
    const valueArray: string[] = castArray(otherProps.value).map((i) => (typeof (i) === 'object' ? i?.id : i)).filter(Boolean);
    return uniq(castArray(ids).concat(valueArray).concat(values).filter((i) => i && !(i === '0' || extraOptionIds.includes(i))));
  }, [JSON.stringify(selected), JSON.stringify(otherProps.value), values, extraOptions]);
  const idsRef = useRef(selectedUserIds);
  const args = useMemo(() => {
    if (selectDataRef.current && selectedUserIds) {
      // 有新的未加载的值，就重新加载，以区分用户选择和自动选择（比如选中了个人筛选）
      const hasNewUnExistValue = selectedUserIds.some((v) => !selectDataRef.current?.find((record) => record.get('id') === v));
      if (hasNewUnExistValue || forceUpdateValue) {
        idsRef.current = selectedUserIds;
      }
    }
    return { selectedUserIds: idsRef.current, queryFilterIds: uniq([...idsRef.current, ...selectedUserLoadedIds]) };
  }, [selectedUserLoadedIds, selectedUserIds, forceUpdateValue]);
  const userRequest: SelectConfig<User>['request'] = useCallback(
    async (requestData) => {
      let res: any;
      if (request) {
        res = await request(requestData);
      } else {
        const { filter, page, requestArgs } = requestData;
        switch (level) {
          case 'project': {
            res = await userApi.project(projectId).getProjectUsers(filter, page, requestArgs?.selectedUserIds, requestArgs?.queryFilterIds, 50, projectId);
            break;
          }
          case 'org': {
            res = await userApi.project(projectId).org(organizationId).getOrgUsers(filter, page, requestArgs?.selectedUserIds, requestArgs?.queryFilterIds, 50);
            break;
          }
          case 'workbench': {
            res = await userApi.project(projectId).org(organizationId).getWorkbenchUsers({ param: filter, page, size: 50 }, { ignoredUserIds: requestArgs?.selectedUserIds });
          }
        }

        res.list = res.list.filter((user: User) => user.enabled);
      }
      requestLoading.current = false;
      // 临时解决组件筛选时
      setFilterWord('filter', requestData.filter);
      return res;
    },
    [level, organizationId, projectId, request, setFilterWord],
  );
  const config = useMemo((): SelectConfig<User> => ({
    name: 'user',
    textField: 'realName',
    valueField: 'id',
    requestArgs: args,
    onOption: (optionData: any) => {
      const optionProps = onOption ? onOption(optionData) : {};
      return { ...optionProps, className: classNames(ellipsisStyles.option, optionProps.className) };
    },
    request: userRequest,
    optionRenderer: optionRenderer || ((user: User) => (user?.headerHidden ? <span>{user?.realName}</span> : (
      <UserTag
        data={user as User}
        style={{ maxWidth: 'unset', width: 'unset' }}
        className={ellipsisStyles.optionWrap}
      />
    ))),
    renderer: optionRenderer || ((user: User) => (user?.headerHidden ? <span>{user?.realName}</span> : <UserTag data={user as User} tooltip={!flat} className={Styles.userWrap} textClassName={Styles.userText} avatarClassName={Styles.userAvatar} />)),
    middleWare: (data) => {
      let newData = [];
      const temp: User[] = [];
      if (selectedUser) {
        (toArray(toJS(selectedUser)).forEach((user) => {
          temp.push({ ...user, id: user?.id && String(user.id) });
        }));
      }
      // 当请求完成后 再将额外选项附加上
      newData = [...((!requestLoading.current ? extraOptions : []) || []), ...data].map((item: User) => ({ ...item, id: String(item.id) })).filter((item) => !includes((excludeIds || []), item.id));
      newData = unionBy<User>(temp, newData, 'id');// 去重
      if (dataRef) {
        Object.assign(dataRef, {
          current: newData,
        });
      }
      if (afterLoad) {
        const afterData = afterLoad(newData);
        newData = Array.isArray(afterData) ? afterData : newData;
      }
      return newData;
    },
  }), [afterLoad, args, dataRef, excludeIds, extraOptions, onOption, optionRenderer, selectedUser, userRequest, flat]);
  const props = useSelect(config);
  selectDataRef.current = props.options;

  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={refsBindRef(ref, selectRef)}
      clearButton={false}
      {...props}
      {...otherProps}
      popupCls={classNames(ellipsisStyles.popup, otherProps.popupCls)}
    />
  );
});
SelectUser.displayName = 'SelectUser';
export default SelectUser;
