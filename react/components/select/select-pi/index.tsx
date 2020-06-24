import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { FragmentForSearch } from '@/hooks/useSelect';
import { piApi } from '@/api';
import styles from './index.less';

const renderPi = (pi: any) => {
  if (pi) {
    return (
      <div style={{ display: 'inline-block' }}>
        {`${pi.code}-${pi.name}`}
        {
          pi.statusCode === 'doing' && (
            <div className={styles.current}>当前</div>
          )
        }
      </div>
    );
  }
  return null;
};
interface Props extends SelectProps {
  statusList: string[]
  multiple?: boolean
}
const SelectPI: React.FC<Props> = forwardRef(({
  statusList, multiple, ...otherProps 
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<PI> => ({
    name: 'all_pi',
    textField: 'piName',
    valueField: 'id',
    request: () => piApi.getPiListByStatus(statusList),
    optionRenderer: pi => (
      <FragmentForSearch name={`${pi.code}-${pi.name}`}>
        {renderPi(pi)}
      </FragmentForSearch>
    ),
    paging: false,
  }), [JSON.stringify(statusList)]);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectPI;
