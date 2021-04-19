import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import {
  featureApi, issueTypeApi, IStatusCirculation, statusTransformApi,
} from '@/api';
import { observer } from 'mobx-react-lite';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IIssueType } from '@/common/types';

interface Props extends Partial<SelectProps> {
  featureIds?: number[],
  afterLoad?: (features: any[]) => void
}
const FeatureProjectField: React.FC<Props> = forwardRef(({ featureIds, afterLoad, ...otherProps }, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IIssueType> => ({
    name: 'featureId',
    textField: 'summary',
    valueField: 'issueId',
    request: ({ filter, page }) => featureApi.queryAllInSubProject(featureIds || [], filter!, page, 10),
    middleWare: (data) => {
      if (afterLoad) {
        afterLoad(data);
      }
      return data;
    },
  }), []);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default FeatureProjectField;
