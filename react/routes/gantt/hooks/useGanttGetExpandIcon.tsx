import { usePersistFn } from 'ahooks';
import React from 'react';
import GanttExpandIcon from '../components/gantt-expand-icon';

function useGanttGetExpandIcon() {
  const getExpandIcon = usePersistFn(({
    level, index, collapsed, onClick,
  }) => (
    <GanttExpandIcon collapsed={collapsed} wrapElementProps={{ onClick }} />
  ));
  return getExpandIcon;
}
export default useGanttGetExpandIcon;
