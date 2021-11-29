import React, { useCallback } from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import ToggleFieldValue from '@/routes/page-config/components/toggle-field-value';
import { usePageIssueTypeStore } from '../../stores';
import { useSortTableContext } from './stores';
import DraggableIBaseItem from './DraggableIBaseItem';
import useFormatMessage from '@/hooks/useFormatMessage';
// import CheckBox from './components/Checkbox';

interface Props {
  data: Record,
  provided: any,
  virtualizedStyle?: React.CSSProperties,
  draggingClassName?: string,
  isDragDisabled?: boolean,
}

const DraggableItem: React.FC<Props> = ({
  data, isDragDisabled, virtualizedStyle, provided, draggingClassName,
}) => {
  const { pageIssueTypeStore } = usePageIssueTypeStore();
  const formatMessage = useFormatMessage();
  const {
    showSplitLine /** 组织层与项目层都使用表格线 */, isProject, prefixCls: originPrefixCls,
  } = useSortTableContext();
  const prefixCls = `${originPrefixCls}-drag`;

  const renderLastColumn = useCallback(() => {
    const createdLevel = data.get('createdLevel');
    if (isProject) {
      return (
        <div
          role="none"
          className={`${prefixCls}-item ${prefixCls}-item-text`}
          {...provided.dragHandleProps}
        >
          {formatMessage({ id: `agile.common.${createdLevel}` })}
        </div>
      );
    }

    return (
      <div
        role="none"
        className={`${prefixCls}-item ${prefixCls}-item-text`}
      >
        <ToggleFieldValue data={data} />
      </div>
    );
  }, [data, formatMessage, isProject, prefixCls, provided.dragHandleProps]);

  const renderColumns = useCallback((nodes: any[]) => {
    nodes.splice(1, 0, renderLastColumn());
    return nodes;
  }, [renderLastColumn]);
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
