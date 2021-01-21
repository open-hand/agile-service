import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { issueTypeApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IIssueType } from '@/common/types';

interface Props extends Partial<SelectProps> {
  dataRef?: React.MutableRefObject<IIssueType[]>
  afterLoad?: (res: IIssueType[]) => void
}

const SelectLinkType: React.FC<Props> = forwardRef(({ dataRef, afterLoad, ...otherProps }) => {
  const config = useMemo((): SelectConfig<IIssueType> => ({
    name: 'linkType',
    textField: 'name',
    valueField: 'id',
    request: ({ page }) => issueTypeApi.getReferencedList(page, 20),
    middleWare: (res) => {
      const data = res || [];
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
  }), [afterLoad, dataRef]);
  const props = useSelect(config);
  return (
    <Select
      {...props}
      {...otherProps}
    />
  );
});
export default SelectLinkType;
