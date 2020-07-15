import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { issueLabelApi } from '@/api';

interface Props {
  dataRef: React.RefObject<Array<any>>
}

const SelectLabel: React.FC<Props> = forwardRef(({ dataRef, ...otherProps }, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'label',
    textField: 'labelName',
    valueField: 'labelName',
    request: () => issueLabelApi.loads(),
    middleWare: (data) => {
      // @ts-ignore
      // eslint-disable-next-line no-param-reassign
      dataRef.current = data;
      return data;
    },
    paging: false,
  }), []);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      multiple
      combo
      {...props}
      {...otherProps}
    />
  );
});
export default SelectLabel;
