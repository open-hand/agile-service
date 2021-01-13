import React from 'react';
import { observer } from 'mobx-react-lite';
import styles from './Comment.less';

interface Props {

}

const Comment: React.FC<Props> = () => (
  <div className={styles.comment}>
    我是单个评论
  </div>
);

export default observer(Comment);
