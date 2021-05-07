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
import { IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import ColumnList from './components/column-list';

export interface Option {
  code: string, title: string
}
export interface ColumnManageProps {
  modal?: IModalProps,
  options: Option[]
  value?: string[]
  onChange?: (value: string[]) => void
}

const ColumnManageModal: React.FC<ColumnManageProps> = (props) => {
  const { modal } = props;

  const { options } = props;
  const [columns, setColumns] = useState(options);
  const allKeys = useMemo(() => columns.map((c) => c.code), [columns]);
  const [selectedKeys, setSelectedKeys] = useState<string[]>(props.value ?? []);

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
    console.log(intersection(allKeys, selectedKeys));
    // 保证选择列之后，列不会到最后一个
    props.onChange && props.onChange(intersection(allKeys, selectedKeys));
    return true;
  });
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <div>
      <DragDropContext
        onDragEnd={handleDragEnd}
      >
        <ColumnList
          columns={columns}
          selectedKeys={selectedKeys}
          onSelectChange={handleSelectChange}
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
export default openColumnManageModal;
