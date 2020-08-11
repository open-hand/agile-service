import { IStatus } from '@/common/types';

const STATUS_TYPE: {
  [key in IStatus['valueCode']]: {
    color: string
    name: string
  }
} = {
  todo: { color: '#ffb100', name: '待处理' },
  doing: { color: '#4d90fe', name: '进行中' },
  done: { color: '#00bfa5', name: '已完成' },
  prepare: { color: '#F67F5A', name: '准备' },
};
export default STATUS_TYPE;
