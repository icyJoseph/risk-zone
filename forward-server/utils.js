const dateAndTime = () => {
  const today = new Date();

  const minutes = today.getMinutes();
  const hours = today.getHours();

  const day = today.getDate();
  const month = today.getMonth();
  const year = today.getFullYear();

  const time = hours < 10 ? `0${hours}:${minutes}` : `${hours}:${minutes}`;

  const DD = day < 10 ? `0${day}` : `${day}`;
  const MM = month < 9 ? `0${month + 1}` : `${month + 1}`;
  const YYYY = year;

  const date = `${YYYY}-${MM}-${DD}`;

  return { date, time };
};

module.exports = { dateAndTime };
