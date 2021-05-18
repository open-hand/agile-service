import React, {
  useMemo, forwardRef, useEffect, useRef,
} from 'react';
import { Select, Icon } from 'choerodon-ui/pro';
import { Observer, useComputed, useForceUpdate } from 'mobx-react-lite';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { devOpsApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ILabel } from '@/common/types';
import { FlatSelect } from '@choerodon/components';

interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  ref?: any
  valueField?: string
  enabledTag?: boolean /** @default 'true' */
  afterLoad?: (sprints: ILabel[]) => void
  applicationId?: string | null
  issueId: string
  flat?: boolean
  projectId?: string

}

const SelectBranch: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, issueId, applicationId, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<{branchName:string, [propsName:string]:any}> => ({
    name: 'branch',
    textField: 'branchName',
    valueField: 'branchName',
    optionRenderer: (branch) => (
      <div>
        <Icon type="branch" className="c7nagile-name-icon" />
        {branch.branchName}
      </div>
    ),
    request: ({ page, filter }) => (applicationId ? devOpsApi.project(projectId).loadBranchesByServiceFilterIssue(applicationId, page, 20, {
      searchParam: {
        branchName: filter,
      },
      param: '',
    }, issueId) : (() => new Promise([] as any))),
    middleWare: (data: any) => {
      if (dataRef) {
        Object.assign(dataRef, {
          current: data,
        });
      }
      if (afterLoad) {
        afterLoad(data);
      }
      return data;
    },
    paging: true,
  }), [applicationId, projectId]);
  const props = useSelect(config);
  useEffect(() => {
    console.log('Component useEffect into', applicationId);
    return () => console.log('leave Component');
  }, [applicationId]);
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
    console.log('forceUpdate applicationId', applicationId);
  }, [applicationId]);
  function handleBindRef(r: any) {
    console.log('r...', r?.record, ref);
    Object.assign(innerRef, { current: r });
    typeof (ref) === 'function' ? ref(r) : ref && Object.assign(ref, { current: r });
  }
  if (!issueId || !applicationId) {
    return <Select {...otherProps} />;
  }

  return <SelectBranch ref={handleBindRef} issueId={issueId} applicationId={applicationId} projectId={projectId} {...otherProps} key={`select-git-tag-${applicationId}-${projectId}`} />;
});
export default SelectSelectBranchHOC;
