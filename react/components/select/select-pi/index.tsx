import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import useSelect, { SelectConfig, FragmentForSearch } from '@/hooks/useSelect';

import { piApi } from '@/api';
import type { PI } from '@/common/types';
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
interface Props extends Partial<SelectProps> {
  statusList: string[]
  afterLoad?: (sprints: PI[]) => void
  multiple?: boolean
  disabledCurrentPI?: boolean
}
const SelectPI: React.FC<Props> = forwardRef(({
  statusList, multiple, disabledCurrentPI = false, afterLoad, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<Function>();
  afterLoadRef.current = afterLoad;
  const config = useMemo((): SelectConfig<PI> => ({
    name: 'all_pi',
    textField: 'piName',
    valueField: 'id',
    request: () => piApi.getPiListByStatus(statusList),
    optionRenderer: (pi) => (
      <FragmentForSearch name={`${pi.code}-${pi.name}`}>
        {renderPi(pi)}
      </FragmentForSearch>
    ),
    middleWare: (sprints) => {
      if (afterLoadRef.current) {
        afterLoadRef.current(sprints);
      }
      return sprints;
    },
    props: {
      // @ts-ignore
      onOption: ({ record }) => {
        if (disabledCurrentPI && record.get('statusCode') === 'doing') {
          return {
            disabled: true,
          };
        }
        return {};
      },
    },
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
