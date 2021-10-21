import { getProjectId } from '@/utils/common';

export function timeTransformWorkHour(time: number, unit: 'H' | 'D' | 'W') {
  if (unit === 'H') {
    return time;
  }
  const unitMap = {
    D: 8,
    W: 40,
  };
  if (Object.keys(unitMap).includes(unit)) {
    return time * unitMap[unit];
  }
  return 0;
}
export function processBeforeData(data: any) {
  const {
    startDate, spendTime, timeUnit, remain, description, projectId,
  } = data;
  const TYPE = {
    auto: 'self_adjustment',
    none: 'no_set_prediction_time',
    direct: 'set_to',
    reduce: 'reduce',
  };
  return {
    projectId: projectId ?? getProjectId(),
    startDate,
    workTime: timeTransformWorkHour(spendTime, timeUnit),
    residualPrediction: TYPE[remain as keyof typeof TYPE],
    predictionTime: ['direct', 'reduce'].includes(remain) ? timeTransformWorkHour(data[`${remain}_time`], data[`${remain}_unit`]) : undefined,
    description,
  };
}
