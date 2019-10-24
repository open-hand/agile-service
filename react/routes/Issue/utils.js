export default function transform(data) {
  const {
    issueTypeId, 
    assigneeId, 
    statusId, 
    issueIds, 
    quickFilterIds, 
    createStartDate, 
    createEndDate, 
    contents,
    component,
    epic,
    label,
    reporter,
    sprint,
    summary,
    version,
  } = data;
  return {
    advancedSearchArgs: {
      issueTypeId,
      // assigneeIds,
      statusId,              
    },     
    otherArgs: {
      assigneeId,
      issueIds,
      component,
      epic,
      label,
      reporter,
      sprint,
      summary,
      version,
    },
    searchArgs: {
      createStartDate,
      createEndDate,           
    },
    quickFilterIds,
    contents,
  };
}
