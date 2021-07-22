import React, { useCallback } from 'react';
import {
  DraggingStyle, NotDraggingStyle,
} from 'react-beautiful-dnd';
import classnames from 'classnames';
import {
  Icon, Tooltip, DataSet, Modal, Button,
} from 'choerodon-ui/pro';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { injectIntl } from 'react-intl';
import { Observer, observer } from 'mobx-react-lite';
import TableDropMenu from '@/components/table-drop-menu';
import CheckBox from '@/components/check-box';
import TextEditToggle from '@/components/TextEditTogglePro';
import { usePageIssueTypeStore } from '../../stores';
import { useSortTableContext } from './stores';
import useTextEditTogglePropsWithPage from './useTextEditToggle';
import DraggableIBaseItem from './DraggableIBaseItem';
// import CheckBox from './components/Checkbox';

interface Props {
  data: Record,
  provided: any,
  virtualizedStyle?: React.CSSProperties,
  draggingClassName?: string,
  isDragDisabled?: boolean,
}
function SpanPlaceholder({ fieldType }: { fieldType: string }) {
  let placeholder = '请选择';
  if (['input', 'number', 'text'].includes(fieldType)) {
    placeholder = '请输入';
  }
  return <span style={{ color: 'rgba(0,0,0,0.6)', fontStyle: 'italic' }}>{placeholder}</span>;
}
export const DraggableOrgEditItem: React.FC<Pick<Props, 'data'>> = ({
  data,
}) => {
  const { pageIssueTypeStore } = usePageIssueTypeStore();
  const {
    isProject, prefixCls: originPrefixCls,
  } = useSortTableContext();
  const prefixCls = `${originPrefixCls}-drag`;

  const textEditToggleProps = useTextEditTogglePropsWithPage(data, isProject, { className: `${prefixCls}-item-defaultValue`, disabled: !pageIssueTypeStore.currentIssueType.enabled });

  return (
    <div
      role="none"
      className={`${prefixCls}-item ${prefixCls}-item-text`}
    >
      <TextEditToggle
        {...textEditToggleProps}
      >

        <Observer>
          {() => (
            <Tooltip title={data.get('showDefaultValueText') !== '' ? data.get('showDefaultValueText') : undefined}>
              <span className={`${prefixCls}-item-defaultValue-text`}>
                {(!textEditToggleProps?.disabled && (!data.get('showDefaultValueText') || data.get('showDefaultValueText') === '') ? <SpanPlaceholder fieldType={data.get('fieldType')} /> : data.get('showDefaultValueText') || '')}
              </span>
            </Tooltip>
          )}
        </Observer>
      </TextEditToggle>
    </div>
  );
};
const DraggableItem: React.FC<Props> = ({
  data, isDragDisabled, virtualizedStyle, provided, draggingClassName,
}) => {
  const { pageIssueTypeStore, intl } = usePageIssueTypeStore();
  const {
    showSplitLine /** 组织层与项目层都使用表格线 */, isProject, prefixCls: originPrefixCls,
  } = useSortTableContext();
  const prefixCls = `${originPrefixCls}-drag`;
  const renderFieldOrigin = useCallback(() => {
    const createdLevel = data.get('createdLevel');
    return (
      <div
        role="none"
        className={`${prefixCls}-item ${prefixCls}-item-text`}
        {...provided.dragHandleProps}
      >
        {intl.formatMessage({ id: createdLevel })}
      </div>
    );
  }, [data, intl, prefixCls, provided.dragHandleProps]);

  const renderColumns = useCallback((nodes: any[]) => {
    nodes.splice(1, 0,
      isProject ? renderFieldOrigin() : <DraggableOrgEditItem data={data} />);
    return nodes;
  }, [data, isProject, renderFieldOrigin]);
  return (
    <DraggableIBaseItem
      data={data}
      provided={provided}
      draggingClassName={draggingClassName}
      isDragDisabled={isDragDisabled}
      virtualizedStyle={virtualizedStyle}
      renderColumns={renderColumns}
    />
  );
};
export default observer(DraggableItem);
