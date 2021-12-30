import React, { useMemo, forwardRef, useRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';
import classNames from 'classnames';
import { Permission, stores } from '@choerodon/boot';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { omit } from 'lodash';
// @ts-ignore
import { PermissionProvider } from '@choerodon/master';
import { Provider } from 'mobx-react';
import useSelect, { SelectConfig, FragmentForSearch, LoadConfig } from '@/hooks/useSelect';
import { piApi } from '@/api';

import type { PI } from '@/common/types';
import renderEllipsisBlockOption, { styles } from '../common/utils';
import useFormatMessage from '@/hooks/useFormatMessage';

const { AppState } = stores;

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
const SelectPI = forwardRef<Select, SelectPIProps>(({
  dataRef, statusList, disabledCurrentPI = false, afterLoad, request, flat, addPi0, doingIsFirst, popupCls, maxTagTextLength = 10, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<SelectPIProps['afterLoad']>();
  const formatMessage = useFormatMessage('agile.common');
  afterLoadRef.current = afterLoad;
  const config = useMemo((): SelectConfig<PI & { piName?: string }> => ({
    name: 'all_pi',
    textField: 'piName',
    valueField: 'id',
    tooltip: true,
    request: request || (() => piApi.project(projectId).getPiListByStatus(statusList)),
    optionRenderer: (pi, tooltip) => {
      const piName = pi.piName || `${pi.code}-${pi.name}`;
      return (
        <FragmentForSearch name={piName}>
          {renderEllipsisBlockOption(piName, <>{formatMessage({ id: 'current' })}</>, { tooltip: true, showBlock: pi.statusCode === 'doing' })}
        </FragmentForSearch>
      );
    },
    renderer: (pi) => renderEllipsisBlockOption(pi.piName || `${pi.code}-${pi.name}`, <>{formatMessage({ id: 'current' })}</>, { tooltip: false, maxLength: maxTagTextLength, showBlock: pi.statusCode === 'doing' }),
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
      return addPi0 ? [{ id: '0', name: '未分配PI', piName: '未分配PI' } as unknown as PI, ...sortPiList] : sortPiList;
    },
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
    paging: false,
  }), [JSON.stringify(statusList), disabledCurrentPI]);
  const props = useSelect(config);

  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      popupCls={classNames(styles.popup, popupCls)}
      dropdownMatchSelectWidth={false}
      maxTagTextLength={maxTagTextLength}
      maxTagCount={3}
      {...props}
      {...otherProps}
    />
  );
});
SelectPI.displayName = 'SelectBasePI';
export interface SelectPIPermissionHOCProps extends SelectPIProps {
  /** 开启权限校验，会自动对涉及权限的选项进行相关操作  @default false */
  openPermission?: boolean

}
function wrapSelectPIPermissionHOC(Element: React.FC<SelectPIProps>) {
  class SelectPIPermission extends React.PureComponent<SelectPIPermissionHOCProps & { selectPIRef?: React.Ref<Select> }> {
    getProps() {
      return { ...omit(this.props, ['openPermission', 'selectPIRef']), ref: this.props.selectPIRef };
    }

    getRef() {
      return this.props.selectPIRef;
    }

    render() {
      if (!this.props.openPermission) {
        return <Element {...this.getProps()} />;
      }
      return (
        <PermissionProvider>

          <Provider AppState={AppState}>
            <Permission service={[
              'choerodon.code.project.plan.feature.ps.choerodon.code.project.plan.feature.completepi',
              'choerodon.code.project.plan.feature.ps.choerodon.code.project.plan.feature.startpi',
              'choerodon.code.project.plan.feature.ps.pi.plan',
            ]}
            >
              {(hasPermission: boolean) => (
                <Element {...this.getProps()} disabledCurrentPI={!hasPermission} />
              )}
            </Permission>
          </Provider>
        </PermissionProvider>
      );
    }
  }

  function forwardPIRef(props: SelectPIPermissionHOCProps, ref: React.Ref<Select>) {
    return <SelectPIPermission {...props} selectPIRef={ref} />;
  }
  const name = Element.displayName || Element.name;
  forwardPIRef.displayName = `SelectPIPermissionHOC(${name})`;
  return React.forwardRef(forwardPIRef);
}
export default wrapSelectPIPermissionHOC(SelectPI);
