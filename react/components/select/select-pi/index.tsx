import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import useSelect, { SelectConfig, FragmentForSearch, LoadConfig } from '@/hooks/useSelect';
import { piApi } from '@/api';
import type { PI } from '@/common/types';
import styles from './index.less';

const renderPi = (pi: any) => {
  if (pi) {
    return (
      <div style={{ display: 'inline-block' }}>
        {pi.id === '0' ? pi.name : pi.fullName || `${pi.code}-${pi.name}`}
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
export interface SelectPIProps extends Partial<SelectProps> {
  statusList?: string[]
  afterLoad?: (piList: PI[]) => void
  request?: ({ filter, page }: LoadConfig) => Promise<PI[]>
  multiple?: boolean
  disabledCurrentPI?: boolean
  dataRef?: React.MutableRefObject<any>
  flat?: boolean
  addPi0?: boolean
  doingIsFirst?: boolean
  projectId?: string
}
const SelectPI: React.FC<SelectPIProps> = forwardRef(({
  dataRef, statusList, disabledCurrentPI = false, afterLoad, request, flat, addPi0, doingIsFirst, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<SelectPIProps['afterLoad']>();
  afterLoadRef.current = afterLoad;
  const config = useMemo((): SelectConfig<PI & { piName?: string }> => ({
    name: 'all_pi',
    textField: 'piName',
    valueField: 'id',
    request: request || (() => piApi.project(projectId).getPiListByStatus(statusList)),
    optionRenderer: (pi) => (
      <FragmentForSearch name={pi.id === '0' ? pi.name : pi.piName || `${pi.code}-${pi.name}`}>
        {renderPi(pi)}
      </FragmentForSearch>
    ),
    afterLoad: afterLoadRef.current,
    middleWare: (piList) => {
      let sortPiList = [...piList];
      const doingPi = piList.find((item) => item.statusCode === 'doing');
      if (doingPi && doingIsFirst) {
        sortPiList = [doingPi, ...piList.filter((item) => item.statusCode !== 'doing')];
      }
      if (dataRef) {
        Object.assign(dataRef, {
          current: sortPiList,
        });
      }
      return addPi0 ? [{ id: '0', name: '未分配PI' } as unknown as PI, ...sortPiList] : sortPiList;
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

  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      dropdownMenuStyle={{ maxWidth: '3rem' }}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectPI;
