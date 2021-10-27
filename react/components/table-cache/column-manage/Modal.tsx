import React, {
  useEffect, useCallback, useMemo, useState,
} from 'react';
import {
  Modal,
  Menu, Dropdown, Button,
} from 'choerodon-ui/pro';
import { useControllableValue, usePersistFn } from 'ahooks';
import { DragDropContext, DropResult } from 'react-beautiful-dnd';
import { intersection } from 'lodash';
import { ModalProps } from 'choerodon-ui/pro/lib/modal/Modal';
import { IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { useUpdateColumnMutation } from '@/hooks/data/useTableColumns';
import ColumnList from './components/column-list';
import styles from './Modal.less';
import WarnInfoBlock, { IWarnInfoBlockProps } from '@/components/warn-info-block';

export interface Option {
  code: string, title: string, disabled?: boolean
}
export interface ColumnManageProps {
  type: string
  modal?: IModalProps,
  options: Option[]
  value?: string[]
  projectId?: string
  onChange?: (value: string[]) => void
  tooltip?: boolean
}
function useColumnManageModal(props: ColumnManageProps) {
  const { modal, type, tooltip = true } = props;

  const { options } = props;
  const [columns, setColumns] = useState([...options]);
  const allKeys = useMemo(() => columns.map((c) => c.code), [columns]);
  const [selectedKeys, setSelectedKeys] = useState<string[]>(props.value ?? []);
  const mutation = useUpdateColumnMutation(type, props.projectId);
  const updateSelectKeys = usePersistFn((keys: string[]) => {
    setSelectedKeys(keys);
  });
  const choose = useCallback((key) => {
    updateSelectKeys([...selectedKeys, key]);
  }, [selectedKeys, updateSelectKeys]);
  const unChoose = useCallback((key) => {
    updateSelectKeys(selectedKeys.filter((k) => k !== key));
  }, [selectedKeys, updateSelectKeys]);
  const handleSelectChange = useCallback((
    key,
    value,
  ) => {
    if (value) {
      choose(key);
    } else {
      unChoose(key);
    }
  }, [choose, unChoose]);
  const handleDragEnd = usePersistFn((result: DropResult) => {
    if (result.source && result.destination) {
      const { source: { index: sourceIndex }, destination: { index: destinationIndex } } = result;
      const [moved] = columns.splice(sourceIndex, 1) ?? [];
      if (moved) {
        columns.splice(destinationIndex, 0, moved);
      }
      setColumns([...columns]);
    }
  });
  const handleSubmit = usePersistFn(async () => {
    // 保证选择列之后，列不会到最后一个
    // props.onChange && props.onChange(intersection(allKeys, selectedKeys));
    const codes = intersection(allKeys, selectedKeys);
    await mutation.mutateAsync({
      applyType: type,
      listLayoutColumnRelVOS: columns.map((column, i) => ({
        // TODO: 字段id
        // fieldId:
        columnCode: column.code,
        display: selectedKeys.includes(column.code),
        sort: i,
        width: 0,
      })),
    });
    return true;
  });
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return useMemo(() => ({
    onDragEnd: handleDragEnd,
    columListProps: {
      tooltip,
      columns,
      selectedKeys,
      onSelectChange: handleSelectChange,
    },
  }), [columns, handleDragEnd, handleSelectChange, selectedKeys, tooltip]);
}
const ColumnManageModal: React.FC<ColumnManageProps> = (props) => {
  const { onDragEnd, columListProps } = useColumnManageModal(props);
  return (
    <div>
      <DragDropContext
        onDragEnd={onDragEnd}
      >
        <ColumnList
          {...columListProps}
        />
      </DragDropContext>
    </div>
  );
};

const openColumnManageModal = (props: ColumnManageProps) => {
  Modal.open({
    key: 'ColumnManageModal',
    title: '列表显示设置',
    drawer: true,
    style: {
      width: MODAL_WIDTH.small,
    },
    children: <ColumnManageModal {...props} />,
  });
};
interface CustomColumnManageProps extends ColumnManageProps {
  modelProps?: Omit<ModalProps, 'children'>
  warnInfoProps?: IWarnInfoBlockProps
}
const CustomColumnManageModal: React.FC<CustomColumnManageProps> = (props) => {
  const { onDragEnd, columListProps } = useColumnManageModal(props);
  return (
    <div>
      <WarnInfoBlock
        className={styles.prompt}
        visible
        iconHidden
        style={{ padding: 10, paddingLeft: 22 }}
        content="为了更好的视觉效果,建议您显示字段的数量控制在
        3~6个之间"
        {...props.warnInfoProps}
      />
      <div style={{ marginTop: -14 }}>
        <DragDropContext
          onDragEnd={onDragEnd}
        >
          <ColumnList
            {...columListProps}
          />
        </DragDropContext>
      </div>

    </div>
  );
};

export const openCustomColumnManageModal = (props: CustomColumnManageProps) => {
  Modal.open({
    key: 'ColumnManageModal',
    title: '列表显示设置',
    drawer: true,
    style: {
      width: MODAL_WIDTH.small,
    },
    children: <CustomColumnManageModal {...props} />,
    ...props.modelProps,
  });
};
export default openColumnManageModal;
