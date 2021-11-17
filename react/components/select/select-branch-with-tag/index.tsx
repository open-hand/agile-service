import React, {
  useMemo, forwardRef, useEffect, useRef, useCallback,
} from 'react';
import {
  Select, Icon, DataSet, Tooltip,
} from 'choerodon-ui/pro';
import {
  useForceUpdate,
} from 'mobx-react-lite';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { FlatSelect } from '@choerodon/components';
import { useDebounceFn, usePersistFn } from 'ahooks';
import { ILabel } from '@/common/types';
import { devOpsApi } from '@/api';
import useSelect from '@/hooks/useSelect';

interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  ref?: any
  valueField?: string
  enabledTag?: boolean /** @default 'true' 是否开启展示tag */
  afterLoad?: (sprints: ILabel[]) => void
  applicationId?: string | null
  issueId: string
  flat?: boolean
  projectId?: string

}
function useGetBranchTagData({
  enabledTag, applicationId, projectId, issueId, afterLoad, dataRef,
}: Pick<Props, 'applicationId' | 'enabledTag' | 'projectId' | 'issueId' | 'afterLoad' | 'dataRef'>) {
  const branchProps = useSelect({
    valueField: 'branchName',
    textField: 'branchName',
    optionRenderer: (branch) => (
      <div>
        <Icon type="branch" className="c7nagile-name-icon" />
        {branch.branchName}
      </div>
    ),
    request: ({ page, filter }) => (applicationId ? devOpsApi.project(projectId).loadBranchesByServiceFilterIssue(applicationId, page, 10, {
      searchParam: {
        branchName: filter,
      },
      param: '',
    }, issueId) : (() => new Promise([] as any))),
  });
  const tagProps = useSelect({
    valueField: 'name',
    textField: 'name',
    optionRenderer: (tag) => (
      <div>
        <Icon type="local_offer" className="c7nagile-name-icon" />
        {tag.name}
      </div>
    ),
    request: ({ page, filter }) => (applicationId && enabledTag ? devOpsApi.project(projectId).loadTagsByService(applicationId, page, 10, {
      searchParam: {
        tagName: filter,
      },
      param: '',
    }) : (() => new Promise([] as any))),
  });
  const handleTransferData = usePersistFn((data: any) => {
    afterLoad && afterLoad(data);
    if (dataRef) {
      Object.assign(dataRef, {
        current: data,
      });
    }
  });

  const optionDataSet = useMemo(() => new DataSet({
    autoCreate: false,
    autoQuery: false,
    paging: false,
    fields: [
      { name: 'meaning', type: 'string' as any },
      { name: 'value', type: 'string' as any },
      { name: 'groupType', type: 'string' as any, group: enabledTag },
    ],
  }), [enabledTag]);
  const loadData = useCallback(() => {
    const data = [...branchProps.options.map((r) => ({ ...r.toData(), groupType: '分支' })),
      ...tagProps.options.map((r) => ({ ...r.toData(), groupType: 'Tag' }))];
    handleTransferData(data);
    optionDataSet.loadData(data);
    // return data;
  }, [branchProps.options, handleTransferData, optionDataSet, tagProps.options]);
  const { run: debounceLoadData } = useDebounceFn(loadData, { wait: 350 });
  useEffect(() => {
    branchProps.options.addEventListener('load', () => {
      debounceLoadData();
    });
    tagProps.options.addEventListener('load', () => {
      debounceLoadData();
    });
  }, [branchProps.options, debounceLoadData, tagProps.options]);
  const handleInput = useCallback((e: any) => {
    branchProps.onInput(e);
    tagProps.onInput(e);
  }, [branchProps, tagProps]);
  const renderOption = useCallback((renderProps: any) => {
    const { record } = renderProps;
    return record.get('groupType') === '分支' ? branchProps.optionRenderer(renderProps) : tagProps.optionRenderer(renderProps);
  }, [branchProps, tagProps]);
  const renderShowInput = useCallback((renderProps: RenderProps) => {
    const { value, text } = renderProps;
    if (!value) {
      return '';
    }
    if (!enabledTag) {
      return branchProps.renderer(renderProps);
    }
    return text;
  }, [branchProps, enabledTag]);
  const props = useMemo(() => ({
    onInput: handleInput,
    options: optionDataSet,
    renderer: renderShowInput,
    searchMatcher: () => true,
    searchable: true,
    optionRenderer: renderOption,
  }), [handleInput, optionDataSet, renderOption, renderShowInput]);
  return props;
}
const SelectBranch: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, issueId, applicationId, enabledTag, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const props = useGetBranchTagData({
    enabledTag, applicationId, projectId, issueId, afterLoad, dataRef,
  });

  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      help={!applicationId ? '请先选择应用服务' : undefined}
      {...props}
      {...otherProps}
    />
  );
});

const SelectSelectBranchHOC: React.FC<Props> = forwardRef(({
  applicationId, issueId, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const innerRef = useRef<Select>(null);
  const forceUpdate = useForceUpdate();

  useEffect(() => {
    forceUpdate();
  }, [applicationId]);
  function handleBindRef(r: any) {
    Object.assign(innerRef, { current: r });
    typeof (ref) === 'function' ? ref(r) : ref && Object.assign(ref, { current: r });
  }
  if (!issueId || !applicationId) {
    return (
      <Tooltip title="请先选择应用服务">
        <div><Select disabled {...otherProps} /></div>
      </Tooltip>
    );
  }

  return <SelectBranch ref={handleBindRef} issueId={issueId} applicationId={applicationId} projectId={projectId} {...otherProps} key={`select-git-tag-${applicationId}-${projectId}`} />;
});
export default SelectSelectBranchHOC;
