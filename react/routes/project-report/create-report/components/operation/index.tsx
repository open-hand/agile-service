import React from 'react';
import { Button } from 'choerodon-ui/pro';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import styles from './index.less';

interface Props {

}
const Operation: React.FC<Props> = () => (
  <div
    className={styles.bar}
  >
    <Button funcType={'raised' as FuncType} color={'blue' as ButtonColor}>保存</Button>
    <Button funcType={'raised' as FuncType}>预览</Button>
    <Button funcType={'raised' as FuncType}>导出</Button>
    <Button funcType={'raised' as FuncType}>发送</Button>
    <Button funcType={'raised' as FuncType}>取消</Button>
  </div>
);
export default Operation;
