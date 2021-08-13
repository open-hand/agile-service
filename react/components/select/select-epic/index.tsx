import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { epicApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { IEpic } from '@/components/charts/epic-report/search';

export interface SelectEpicProps extends Partial<SelectProps> {
  isProgram?: boolean
  request?: SelectConfig<any>['request']
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (epics: IEpic[]) => void
  dontAddEpic0?: boolean
  unassignedEpic?: boolean
  flat?: boolean
  projectId?: string
}

const SelectEpic: React.FC<SelectEpicProps> = forwardRef(({
  isProgram, afterLoad, dataRef, dontAddEpic0, unassignedEpic, request, flat, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'epic',
    textField: 'epicName',
    valueField: 'issueId',
    request: request || (() => (isProgram ? epicApi.project(projectId).loadProgramEpics() : epicApi.project(projectId).loadEpics())),
    middleWare: (epicList: IEpic[]) => {
      if (unassignedEpic || (isProgram && !dontAddEpic0)) {
        epicList.unshift({ issueId: '0', epicName: '未分配史诗' });
      }
      if (dataRef) {
        Object.assign(dataRef, {
          current: (unassignedEpic || (isProgram && !dontAddEpic0)) ? epicList.unshift({ issueId: '0', epicName: '未分配史诗' }) : epicList,
        });
      }
      if (afterLoad) {
        afterLoad(epicList);
      }
      return epicList;
    },
    paging: false,
  }), []);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      clearButton
      {...props}
      {...otherProps}
    />
  );
});
export default SelectEpic;
