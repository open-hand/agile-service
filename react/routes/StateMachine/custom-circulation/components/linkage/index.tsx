import React from 'react';
import { observer } from 'mobx-react-lite';
import style from './index.less';

const Condition = () => (
  <div className={style.condition}>
    状态联动
  </div>
);

export default observer(Condition);
