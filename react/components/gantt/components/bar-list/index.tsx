import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import TaskBar from '../task-bar';
import InvalidTaskBar from '../invalid-task-bar';
import Context from '../../context';

const BarList: React.FC = () => {
  const { store } = useContext(Context);
  const barList = store.getBarList;
  const { count, start } = store.getVisibleRows;
  return (
    <>
      {barList.slice(start, start + count).map((bar, index) => {
        const rowIndex = index + start;
        return bar.invalidDateRange ? (
          <InvalidTaskBar
            // eslint-disable-next-line react/no-array-index-key
            key={`${bar.label}-${rowIndex}-invalid`}
            data={bar}
          />
        ) : (
          <TaskBar
            // eslint-disable-next-line react/no-array-index-key
            key={`${bar.label}-${rowIndex}`}
            data={bar}
          />
        );
      })}
    </>
  );
};
export default observer(BarList);
