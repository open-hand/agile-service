import React, { useMemo, forwardRef, useRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';
import classNames from 'classnames';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import useSelect, { SelectConfig, FragmentForSearch, LoadConfig } from '@/hooks/useSelect';
import { piApi } from '@/api';
import type { PI } from '@/common/types';
import styles from './index.less';

const renderPi = (pi: any, maxLength?: number, tooltip?: boolean) => {
  if (pi) {
    const name = pi.id === '0' ? pi.name : pi.fullName || `${pi.code}-${pi.name}`;
    const suffix = name && maxLength && String(name).length > maxLength ? '...' : undefined;
    const piItem = (
      <div className={classNames(styles.option_wrap, { [styles.option_wrap_suffix]: !!maxLength })}>
        <span className={classNames({ [styles.ellipsis]: !maxLength })}>{name?.slice(0, maxLength)}</span>
        {suffix}
        {
          pi.statusCode === 'doing' && (
            <div className={styles.current}>当前</div>
          )
        }
      </div>
    );
    return tooltip
      ? (
        <Tooltip title={name} placement="topLeft" arrowPointAtCenter style={{ zIndex: 9999 }}>
          {piItem}
        </Tooltip>
      ) : piItem;
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
  dataRef, statusList, disabledCurrentPI = false, afterLoad, request, flat, addPi0, doingIsFirst, popupCls, maxTagTextLength = 10, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<SelectPIProps['afterLoad']>();
  afterLoadRef.current = afterLoad;
  const config = useMemo((): SelectConfig<PI & { piName?: string }> => ({
    name: 'all_pi',
    textField: 'piName',
    valueField: 'id',
    tooltip: true,
    request: request || (() => piApi.project(projectId).getPiListByStatus(statusList)),
    optionRenderer: (pi, tooltip) => {
      const piName = pi.id === '0' ? pi.name : pi.piName || `${pi.code}-${pi.name}`;
      return (
        <FragmentForSearch name={piName}>
          {renderPi(pi, undefined, tooltip)}
        </FragmentForSearch>
      );
    },
    renderer: (item) => renderPi(item, maxTagTextLength, false)!,
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
        const common = { className: styles.option };
        if (disabledCurrentPI && record.get('statusCode') === 'doing') {
          return {
            disabled: true,
            ...common,
          };
        }
        return common;
      },
    },
    paging: false,
  }), [JSON.stringify(statusList)]);
  const props = useSelect(config);

  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      popupCls={classNames(styles.pi, popupCls)}
      dropdownMatchSelectWidth={false}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectPI;
