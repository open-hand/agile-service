import { IStatus } from '@/common/types';

const STATUS_COLOR: {
  [key in IStatus['valueCode']]: string[]
} = {
  todo: ['#ffb100', '#FFEFCC'],
  doing: ['#4d90fe', '#D8E7FF'],
  done: ['#00bfa5', '#DFFFFB'],
  prepare: ['#F67F5A', '#F67F5A'],
};
export default STATUS_COLOR;
