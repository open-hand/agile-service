import React, {
  useMemo, forwardRef, useState, useReducer, useEffect, useCallback,
} from 'react';
import { Select as OldSelect } from 'choerodon-ui';
import { Select, Tooltip } from 'choerodon-ui/pro';
import { SelectProps as OldSelectProps } from 'choerodon-ui/lib/select';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { versionApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { IProgramVersion } from '@/common/types';
import versionStyles from './index.less';

export interface SelectProgramVersionProps extends Partial<SelectProps> {
  teamProjectIds?: string[],
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (versions: any[]) => void | Array<any>
  flat?: boolean
  optionFlat?: boolean
  filterSelected?: boolean
  projectId?: string
  selectedIds?: string[],
  /**
   * 是否启用查询
   * @default true
   */
  enabledQuery?: boolean
}
interface OldProps extends Partial<OldSelectProps> {
  teamProjectIds?: string[],
}
interface StateProps {
  data: Array<IProgramVersion>
  option: Map<string, Array<IProgramVersion>>
  headOptions: Array<{ id: string, name: string }>
}
interface ActionProps extends Partial<StateProps> {
  type: 'init' | 'change' | 'destroy'
}
interface VersionDataConfigProps {
  dataRef?: React.MutableRefObject<any>
  teamProjectIds?: string[]
  afterLoad?: (versions: any[]) => void | Array<any>
  projectId?: string
  selectedIds?: string[],
  /**
   * 是否启用查询
   * @default true
   */
  enabled?: boolean
}
function useGetVersionData({
  dataRef, afterLoad, teamProjectIds, projectId, selectedIds, enabled = true,
}: VersionDataConfigProps): [StateProps, any] {
  const [versionData, dispatch] = useReducer<(state: StateProps, action: ActionProps) => StateProps>((state, action) => {
    const { type } = action;
    switch (type) {
      case 'init':
        return {
          data: [],
          option: new Map(),
          headOptions: [],
        };
      case 'change': {
        let { data } = action;
        if (dataRef) {
          Object.assign(dataRef, {
            current: data,
          });
        }
        if (afterLoad) {
          data = afterLoad(data || []) || data;
        }
        const newOption = new Map<string, Array<IProgramVersion>>();
        const newHeadOption = new Array<{ id: string, name: string }>();
        data?.forEach((item) => {
          if (newOption.has(item.versionBaseId)) {
            newOption.get(item.versionBaseId)!.push(item);
          } else {
            newHeadOption.push({ id: item.versionBaseId, name: item.versionBase?.name || '' });
            newOption.set(item.versionBaseId, [item]);
          }
        });
        return {
          data: data!,
          headOptions: newHeadOption,
          option: newOption,
        };
      }

      default:
        return state;
    }
  }, { data: [], option: new Map(), headOptions: [] });

  const loadData = useCallback(async () => {
    enabled && versionApi.project(projectId).loadProgramVersion(false, teamProjectIds, selectedIds).then((res: any) => {
      dispatch({ type: 'change', data: res });
    });
  }, [enabled, projectId, teamProjectIds, selectedIds]);
  useEffect(() => { loadData(); }, [loadData]);
  return [versionData, { loadData, dispatch }];
}
const SelectProgramVersion: React.FC<SelectProgramVersionProps> = forwardRef(({
  teamProjectIds, dataRef, afterLoad, flat, optionFlat, projectId, selectedIds, enabledQuery, ...otherProps
}, ref: React.Ref<Select>) => {
  const Component = flat ? FlatSelect : Select;
  const [versionData, method] = useGetVersionData({
    teamProjectIds, dataRef, afterLoad, projectId, selectedIds, enabled: enabledQuery ?? true,
  });
  const OptionComponent = optionFlat ? versionData.data.map((option) => (
    <Component.Option value={option.id}>
      {option.name}
    </Component.Option>
  )) : versionData.headOptions.map((item) => {
    const options = (versionData.option.get(item.id) || []);

    return (
      <Component.OptGroup label={(
        <Tooltip title={item.name}>
          <span className={versionStyles.OptGroup}>{item.name}</span>
        </Tooltip>) as unknown as string}
      >
        {options.map((option) => (
          <Component.Option value={option.id}>
            {option.name}
          </Component.Option>
        ))}
      </Component.OptGroup>
    );
  });
  return (
    <Component
      ref={ref}
      clearButton={false}
      // {...props}
      maxTagCount={3}
      maxTagTextLength={10}
      popupStyle={{ maxWidth: 350 }}
      optionRenderer={({ text }) => <Tooltip title={text}>{text}</Tooltip>}
      {...otherProps}
    >
      {OptionComponent}
    </Component>
  );
});
SelectProgramVersion.displayName = 'SelectProgramVersion';
export default SelectProgramVersion;
const OldSelectProgramVersion: React.FC<OldProps> = ({ teamProjectIds, ...restProps }) => {
  const [versionData, method] = useGetVersionData({ teamProjectIds });
  const OptionComponent = versionData.headOptions.map((item) => {
    const options = (versionData.option.get(item.id) || []);

    return (
      <OldSelect.OptGroup
        key={item.name}
        label={(
          <Tooltip title={item.name}>
            <span className={versionStyles.OptGroup}>{item.name}</span>
          </Tooltip>) as unknown as string}
      >
        {options.map((option) => (
          <OldSelect.Option value={option.id}>
            {option.name}
          </OldSelect.Option>
        ))}
      </OldSelect.OptGroup>
    );
  });
  return (
    <OldSelect
      showCheckAll={false}
      maxTagCount={3}
      {...restProps}
    >
      {OptionComponent}
    </OldSelect>
  );
};
export { OldSelectProgramVersion };
