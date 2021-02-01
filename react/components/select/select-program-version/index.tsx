import React, {
  useMemo, forwardRef, useState, useReducer, useEffect, useCallback,
} from 'react';
import { Select as OldSelect } from 'choerodon-ui';
import { Select, Tooltip } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { versionApi } from '@/api';
import { SelectProps as OldSelectProps } from 'choerodon-ui/lib/select';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import FlatSelect from '@/components/flat-select';
import { IProgramVersion } from '@/common/types';
import versionStyles from './index.less';

interface Props extends Partial<SelectProps> {
  teamProjectIds?: string[],
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (versions: any[]) => void
  flat?: boolean
  filterSelected?: boolean
  projectId?: string
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
  afterLoad?: (versions: any[]) => void
  projectId?: string
}
function useGetVersionData({
  dataRef, afterLoad, teamProjectIds, projectId,
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
        const { data } = action;
        if (dataRef) {
          Object.assign(dataRef, {
            current: data,
          });
        }
        if (afterLoad) {
          afterLoad(data || []);
        }
        const newOption = new Map<string, Array<IProgramVersion>>();
        const newHeadOption = new Array<{ id: string, name: string }>();
        data?.forEach((item) => {
          if (newOption.has(item.versionBase.id)) {
            newOption.get(item.versionBase.id)!.push(item);
          } else {
            newHeadOption.push({ id: item.versionBase.id, name: item.versionBase.name });
            newOption.set(item.versionBase.id, [item]);
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
    versionApi.project(projectId).loadProgramVersion(false, teamProjectIds).then((res: any) => {
      dispatch({ type: 'change', data: res });
    });
  }, [projectId, teamProjectIds]);
  useEffect(() => { loadData(); }, [loadData]);
  return [versionData, { loadData, dispatch }];
}
const SelectProgramVersion: React.FC<Props> = forwardRef(({
  teamProjectIds, dataRef, afterLoad, flat, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const Component = flat ? FlatSelect : Select;
  const [versionData, method] = useGetVersionData({
    teamProjectIds, dataRef, afterLoad, projectId,
  });
  const OptionComponent = versionData.headOptions.map((item) => {
    const options = (versionData.option.get(item.id) || []);

    return (
      <Component.OptGroup label={(
        <Tooltip title={item.name}>
          <span className={versionStyles.OptGroup}>{item.name}</span>
        </Tooltip>) as unknown as string}
      >
        { options.map((option) => (
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
export default SelectProgramVersion;
const OldSelectProgramVersion: React.FC<OldProps> = ({ teamProjectIds, ...restProps }) => {
  const [versionData, method] = useGetVersionData({ teamProjectIds });
  const OptionComponent = versionData.headOptions.map((item) => {
    const options = (versionData.option.get(item.id) || []);

    return (
      <OldSelect.OptGroup label={(
        <Tooltip title={item.name}>
          <span className={versionStyles.OptGroup}>{item.name}</span>
        </Tooltip>) as unknown as string}
      >
        { options.map((option) => (
          <OldSelect.Option value={option.id}>
            {option.name}
          </OldSelect.Option>
        ))}
      </OldSelect.OptGroup>
    );
  });
  return (
    <OldSelect
      maxTagCount={3}
      {...restProps}
    >
      {OptionComponent}
    </OldSelect>
  );
};
export { OldSelectProgramVersion };
