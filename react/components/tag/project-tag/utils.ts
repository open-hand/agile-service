import moment from 'moment';

export function getProjectColor(creationDate: string) {
  const unix = String(moment(creationDate).unix());
  // @ts-ignore
  const index = unix.substring(unix.length - 3) % 4;
  const valiable = [
    'linear-gradient(225deg,rgba(152,229,218,1) 0%,rgba(0,191,165,1) 100%)',
    'linear-gradient(226deg,rgba(255,212,163,1) 0%,rgba(255,185,106,1) 100%)',
    'linear-gradient(226deg,rgba(161,188,245,1) 0%,rgba(104,135,232,1) 100%)',
    'linear-gradient(226deg,rgba(255,177,185,1) 0%,rgba(244,133,144,1) 100%)',
  ];
  return valiable[index];
}
