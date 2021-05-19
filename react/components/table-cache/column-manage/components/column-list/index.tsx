import classNames from 'classnames';
import React, { useCallback } from 'react';
import { Droppable } from 'react-beautiful-dnd';
import { ColumnManageProps } from '../../Modal';
import ColumnItem from '../column-item';
import styles from './index.less';

export interface ColumnListProps {
  columns: ColumnManageProps['options']
  selectedKeys: string[]
  onSelectChange: (key: string, value: boolean) => void
}
const ColumnList: React.FC<ColumnListProps> = ({ columns, selectedKeys, onSelectChange }) => (

  <Droppable droppableId="list" direction="vertical" type="status_drop">
    {(provided, snapshot) => (
      <div
        className={classNames(styles.card_list)}
        ref={provided.innerRef}
        {...provided.droppableProps}
        style={{
          width: '100%',
        }}
      >
        {columns.map((item, index) => {
          const selected = selectedKeys.includes(item.code);
          return (
            <ColumnItem
              selected={selected}
              onSelectChange={onSelectChange}
              key={item.code}
              data={item}
              index={index}
            />
          );
        })}
        {provided.placeholder}
      </div>
    )}
  </Droppable>
);

export default ColumnList;