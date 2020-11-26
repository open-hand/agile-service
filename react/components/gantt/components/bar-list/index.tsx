import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import TaskBar from '../task-bar';
import InvalidTaskBar from '../invalid-task-bar';
import Context from '../../context';

const BarList: React.FC = () => {
  const { store } = useContext(Context);
  const barList = store.getBarList;
  return (
    <>
      {barList.map((bar) => (
        bar.invalidDateRange ? (
          <InvalidTaskBar
            key={`${bar.label}-invalid`}
            data={bar}
          />
        ) : (
          <TaskBar
            key={bar.label}
            data={bar}
          />
        )
      ))}
    </>
  );
};
export default observer(BarList);
