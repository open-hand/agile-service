import React from 'react';
import { observer } from 'mobx-react-lite';
import styles from './Comments.less';

interface Props {

}

const Comments: React.FC<Props> = () => (
  <div className={styles.comment}>
    我是Commentss
  </div>
);

export default observer(Comments);
